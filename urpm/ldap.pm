package urpm::ldap;

# $Id$

use strict;
use warnings;
use urpm;
use urpm::msg 'N';
use Net::LDAP;

our $LDAP_CONFIG_FILE = '/etc/ldap.conf';
my @per_media_opt = (@urpm::PER_MEDIA_OPT, qw(ftp-proxy http-proxy));

# TODO
# use srv dns record ?
# complete the doc

=head1 NAME

urpm::ldap - routines to handle configuration with ldap

=head1 SYNOPSIS

=head1 DESCRIPTION

=over

=item write_ldap_cache($urpm,$medium)

Writes the value fetched from ldap, in case of server failure. This should not
be used to reduce the load of the ldap server, as fetching is still needed, and
therefore, caching is useless if server is up.

=item check_ldap_medium($medium)

Checks if the ldap medium has all required attributes.

=item read_ldap_cache($urpm,%options)

Reads the cache created by the C<write_ldap_cache> function. Should be called
if the ldap server doesn't answer (upgrade, network problem, mobile user, etc.)

=item clean_ldap_cache($urpm)

Cleans the ldap cache, removes all files in the directory.

=item load_ldap_media($urpm,%options)

=item get_ldap_config()

=item get_ldap_config_file($file)

=item get_ldap_config_dns

=back

=cut

sub write_ldap_cache($$) {
    my ($urpm, $medium) = @_;
    my $ldap_cache = "$urpm->{cachedir}/ldap";
    # FIXME what perm for cache ?
    -d $ldap_cache or mkdir $ldap_cache
	or die N("Cannot create ldap cache directory");
    open my $cache, ">", "$ldap_cache/$medium->{name}"
	or die N("Cannot write cache file for ldap\n");
    print $cache "# internal cache file for disconnect ldap operation, do not edit\n";
    foreach (keys %$medium) {
        defined $medium->{$_} or next;
        print $cache "$_ = $medium->{$_}\n";
    }
    close $cache;
    return 1;
}

sub check_ldap_medium($) {
    my ($medium) = @_;
    return $medium->{name} && $medium->{clear_url};
}

sub get_vars_from_sh {
    my $filename = $_[0];
    my %l;
    open my $fh, $filename or return ();
    local $_;
    while (<$fh>) {
	s/#.*//; s/^\s*//; s/\s*$//
	my ($key, $val) = /^(\w+)=(.*)/ or next;
	$val =~ s/^(["'])(.*)\1$/$2/;
	$l{$key} = $val;
    }
    %l;
}

sub read_ldap_cache($%) {
    my ($urpm, %options) = @_;
    foreach (glob("$urpm->{cachedir}/ldap/*")) {
	! -f $_ and next;
	my %medium = get_vars_from_sh($_);
	next if !check_ldap_medium(\%medium);
	$urpm->probe_medium(\%medium, %options) and push @{$urpm->{media}}, \%medium;
    }
}

#- clean the cache, before writing a new one
sub clean_ldap_cache($) {
    my ($urpm) = @_;
    unlink glob("$urpm->{cachedir}/ldap/*");
}

sub get_ldap_config() {
    return get_ldap_config_file($LDAP_CONFIG_FILE);
}

sub get_ldap_config_file($) {
    my ($file) = @_;
    my %config;
    # TODO more verbose error ?
    open my $conffh, $file or return;
    while (<$conffh>) {
	s/#.*//;
	s/^\s*//;
	s/\s*$//;
	s/\s{2}/ /g;
	/^$/ and next;
	/^(\S*)\s*(\S*)/ && $2 or next;
	$config{$1} = $2;
    }
    close($conffh);
    return \%config;
}

sub get_ldap_config_dns {
    # TODO
    die "not implemented now";
}

my %ldap_changed_attributes = (
    'source-name' => 'name',
    'url' => 'clear_url',
    'with-hdlist' => 'with_hdlist',
    'http-proxy' => 'http_proxy',
    'ftp-proxy' => 'ftp_proxy',
);

sub load_ldap_media($%) {
    my ($urpm, %options) = @_;

    my $config = get_ldap_config() or return;

    # try first urpmi_foo and then foo
    foreach my $opt (qw(base uri filter host ssl port binddn passwd scope)) {
        if (!defined $config->{$opt} && defined $config->{"urpmi_$opt"}) {
            $config->{$opt} = $config->{"urpmi_$opt"};
        }
    }

    die N("No server defined, missing uri or host") if !(defined $config->{uri} || defined $config->{host});
    die N("No base defined") if !defined $config->{base};

    if (! defined $config->{uri}) {
        $config->{uri} = "ldap" . ($config->{ssl} eq 'on' ? "s" : "") . "://" .
	    $config->{host} . ($config->{port} ? ":" . $config->{port} : "") . "/";
    }

    my $priority = 100; #- too add ldap media at the end
    eval {
        my $ldap = Net::LDAP->new($config->{uri})
            or die N("Cannot connect to ldap uri :"), $config->{uri};

        $ldap->bind($config->{binddn}, $config->{password})
            or die N("Cannot connect to ldap uri :"), $config->{uri};
        #- base is mandatory
        my $result = $ldap->search(
            base   => $config->{base},
            filter => $config->{filter} || '(objectClass=urpmiRepository)',
            scope  => $config->{scope} || 'sub',
        );

        $result->code and die $result->error;
        # FIXME more than one server ?
        clean_ldap_cache($urpm);

        foreach my $entry ($result->all_entries) {
            my $medium = {};

	    foreach my $opt (@per_media_opt, keys %ldap_changed_attributes) {
		my $v = $entry->get_value($opt);
		defined $v and $medium->{$opt} = $v;
	    }

            #- name is not valid for the schema ( already in top )
            #- and _ are forbidden in attributes names

            foreach (keys %ldap_changed_attributes) {
                $medium->{$ldap_changed_attributes{$_}} = $medium->{$_};
                delete $medium->{$_};
            }
            #- add ldap_ to reduce collision
            #- TODO check if name already defined ?
            $medium->{name} = "ldap_" . $medium->{name};
            $medium->{ldap} = 1;
	    $medium->{priority} = $priority++;
            next if !check_ldap_medium($medium);
            $urpm->probe_medium($medium, %options) and push @{$urpm->{media}}, $medium;
            write_ldap_cache($urpm,$medium) or $urpm->{log}(N("Could not write ldap cache : %s", $_));
        }
    };
    if ($@) {
        $urpm->{log}($@);
        read_ldap_cache($urpm,%options);
    }

}

1;
