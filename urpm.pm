package urpm;

# $Id$

no warnings 'utf8';
use strict;
use File::Find ();
use urpm::msg;
use urpm::download;
use urpm::util;
use urpm::sys;
use urpm::cfg;
use urpm::md5sum;

our $VERSION = '6.10';
our @ISA = qw(URPM Exporter);
our @EXPORT_OK = ('file_from_local_url', 'file_from_local_medium', 'is_local_medium');

use URPM;
use URPM::Resolve;

#- this violently overrides is_arch_compat() to always return true.
sub shunt_ignorearch {
    eval q( sub URPM::Package::is_arch_compat { 1 } );
}

sub xml_info_policies() { qw(never on-demand update-only always) }

sub default_options {
    { 
	'split-level' => 1,
	'split-length' => 8,
	'verify-rpm' => 1,
	'post-clean' => 1,
	'xml-info' => 'on-demand',
	'max-round-robin-tries' => 5,
	'days-between-mirrorlist-update' => 5,
	'nb-of-new-unrequested-pkgs-between-auto-select-orphans-check' => 10,
    };
}

#- create a new urpm object.
sub new {
    my ($class) = @_;
    my $self;
    $self = bless {
	# from URPM
	depslist   => [],
	provides   => {},
	obsoletes  => {},

	media      => undef,
	options    => {},

	fatal      => sub { printf STDERR "%s\n", $_[1]; exit($_[0]) },
	error      => sub { printf STDERR "%s\n", $_[0] },
	info       => sub { printf "%s\n", $_[0] }, #- displayed unless --quiet
	log        => sub { printf "%s\n", $_[0] }, #- displayed if --verbose
	ui_msg     => sub {
	    # deprecated
	    $self->{log}($_[0]);
	},
    }, $class;

    set_files($self, '');
    $self->set_nofatal(1);
    $self;
}

sub new_parse_cmdline {
    my ($class) = @_;
    my $urpm = $class->new;
    urpm::args::parse_cmdline(urpm => $urpm);
    get_global_options($urpm);
    $urpm;
}

sub _add2hash { my ($a, $b) = @_; while (my ($k, $v) = each %{$b || {}}) { defined $a->{$k} or $a->{$k} = $v } $a }

sub get_global_options {
    my ($urpm) = @_;

    my $config = urpm::cfg::load_config($urpm->{config})
      or $urpm->{fatal}(6, $urpm::cfg::err);

    if (my $global = $config->{global}) {
	_add2hash($urpm->{options}, $global);
    }
    #- remember global options for write_config
    $urpm->{global_config} = $config->{global};

    _add2hash($urpm->{options}, default_options());
}

sub prefer_rooted {
    my ($root, $file) = @_;
    -e "$root$file" ? "$root$file" : $file;
}

sub userdir_prefix {
    my ($_urpm) = @_;
    '/tmp/.urpmi-';
}
sub userdir {
    my ($urpm) = @_;
    $< or return;

    my $dir = ($urpm->{urpmi_root} || '') . userdir_prefix($urpm) . $<;
    mkdir $dir, 0755; # try to create it

    -d $dir && ! -l $dir or $urpm->{fatal}(1, N("fail to create directory %s", $dir));
    -o $dir && -w $dir or $urpm->{fatal}(1, N("invalid owner for directory %s", $dir));

    mkdir "$dir/partial";
    mkdir "$dir/rpms";

    $dir;
}
sub ensure_valid_cachedir {
    my ($urpm) = @_;
    if (my $dir = userdir($urpm)) {
	$urpm->{cachedir} = $dir;
    }
    -w "$urpm->{cachedir}/partial" or $urpm->{fatal}(1, N("Can not download packages into %s", "$urpm->{cachedir}/partial"));
}
sub valid_cachedir {
    my ($urpm) = @_;
    userdir($urpm) || $urpm->{cachedir};
}

sub is_temporary_file {
    my ($urpm, $f) = @_;

    begins_with($f, $urpm->{cachedir});
}

sub set_env {
    my ($urpm, $env) = @_;
    -d $env or $urpm->{fatal}(8, N("Environment directory %s does not exist", $env));
    print N("using specific environment on %s\n", $env);
    #- setting new environment.
    $urpm->{config} = "$env/urpmi.cfg";
    if (cat_($urpm->{config}) =~ /^\s*virtual\s*$/m) {
	print "dropping virtual from $urpm->{config}\n";
	system(q(perl -pi -e 's/^\s*virtual\s*$//' ) . $urpm->{config});
    }
    $urpm->{configs_dir} = "$env/media.d";
    $urpm->{skiplist} = "$env/skip.list";
    $urpm->{instlist} = "$env/inst.list";
    $urpm->{prefer_list} = "$env/prefer.list";
    $urpm->{prefer_vendor_list} = "$env/prefer.vendor.list";
    $urpm->{statedir} = $env;
}

sub set_files {
    my ($urpm, $urpmi_root) = @_;

    $urpmi_root and $urpmi_root = file2absolute_file($urpmi_root);

    my %h = (
	config        => "$urpmi_root/etc/urpmi/urpmi.cfg",
	configs_dir   => "$urpmi_root/etc/urpmi/media.d",
	skiplist      => prefer_rooted($urpmi_root, '/etc/urpmi/skip.list'),
	instlist      => prefer_rooted($urpmi_root, '/etc/urpmi/inst.list'),
	prefer_list   => prefer_rooted($urpmi_root, '/etc/urpmi/prefer.list'),
	prefer_vendor_list => 
	                 prefer_rooted($urpmi_root, '/etc/urpmi/prefer.vendor.list'),
	private_netrc => "$urpmi_root/etc/urpmi/netrc",
	statedir      => "$urpmi_root/var/lib/urpmi",
	cachedir      => "$urpmi_root/var/cache/urpmi",
	root          => $urpmi_root,
	$urpmi_root ? (urpmi_root => $urpmi_root) : (),
    );
    $urpm->{$_} = $h{$_} foreach keys %h;

    create_var_lib_rpm($urpm, %h);

   # policy is too use chroot environment only for --urpmi-root, not for --root:
    if ($urpmi_root && -e "$urpmi_root/etc/rpm/macros") {
	URPM::loadmacrosfile("$urpmi_root/etc/rpm/macros");
    }
}

sub create_var_lib_rpm {
    my ($urpm, %h) = @_;
    require File::Path;
    File::Path::mkpath([ $h{statedir}, 
			 (map { "$h{cachedir}/$_" } qw(partial rpms)),
			 dirname($h{config}),
			 "$urpm->{root}/var/lib/rpm",
			 "$urpm->{root}/var/tmp",
		     ]);
}

sub modify_rpm_macro {
    my ($name, $to_remove, $to_add) = @_;

    my $val = URPM::expand('%' . $name);
    $val =~ s/$to_remove/$to_add/ or $val = join(' ', grep { $_ } $val, $to_add);
    URPM::add_macro("$name $val");
}

sub set_tune_rpm {
    my ($urpm, $para) = @_;

    my %h = map { $_ => 1 } map { 
	if ($_ eq 'all') {
	    ('nofsync', 'private');
	} else {
	    $_;
	}
    } split(',', $para);

    $urpm->{tune_rpm} = \%h;
}

sub tune_rpm {
    my ($urpm) = @_;

    if ($urpm->{tune_rpm}{nofsync}) {
	modify_rpm_macro('__dbi_other', 'fsync', 'nofsync');
    }
    if ($urpm->{tune_rpm}{private}) {
	urpm::sys::clean_rpmdb_shared_regions($urpm->{root});
	modify_rpm_macro('__dbi_other', 'usedbenv', 'private');
    }
}

sub _blist_pkg_to_urls {
    my ($blist, @pkgs) = @_;
    my $base_url = $blist->{medium}{url} . '/';
    map { $base_url . $_->filename } @pkgs;
}
sub blist_pkg_to_url {
    my ($blist, $pkg) = @_;
    my ($url) = _blist_pkg_to_urls($blist, $pkg);
    $url;
}
sub blist_to_urls {
    my ($blist) = @_;
    _blist_pkg_to_urls($blist, values %{$blist->{pkgs}});
}
sub blist_to_filenames {
    my ($blist) = @_;
    map { $_->filename } values %{$blist->{pkgs}};
}

sub protocol_from_url {
    my ($url) = @_;
    $url =~ m!^(\w+)(_[^:]*)?:! && $1;
}
sub file_from_local_url {
    my ($url) = @_;
    $url =~ m!^(?:removable[^:]*:/|file:/)?(/.*)! && $1;
}
sub file_from_local_medium {
    my ($medium, $o_url) = @_;
    my $url = $o_url || $medium->{url};
    if ($url =~ m!^cdrom://(.*)!) {
	my $rel = $1;	
	$medium->{mntpoint} or do { require Carp; Carp::confess("cdrom is not mounted yet!\n") };
	"$medium->{mntpoint}/$rel";
    } else {
	file_from_local_url($url);
    }
}
sub is_local_url {
    my ($url) = @_;
    file_from_local_url($url) || is_cdrom_url($url);
}
sub is_local_medium {
    my ($medium) = @_;
    is_local_url($medium->{url});
}
sub is_cdrom_url {
    my ($url) = @_;
    protocol_from_url($url) eq 'cdrom';
}

sub db_open_or_die_ {
    my ($urpm, $b_write_perm) = @_;
    db_open_or_die($urpm, $urpm->{root}, $b_write_perm);
}

sub db_open_or_die__ {
    if ($options{rpmdb}) {
        $db = new URPM;
        $db->parse_synthesis($options{rpmdb});
    } else {
        $db = urpm::db_open_or_die_($urpm);
    }
    $db;
}

# please use higher level function db_open_or_die_()
sub db_open_or_die {
    my ($urpm, $root, $b_write_perm) = @_;

    $urpm->{debug} and $urpm->{debug}("opening rpmdb (root=$root, write=$b_write_perm)");

    my $db = URPM::DB::open($root, $b_write_perm || 0)
      or $urpm->{fatal}(9, N("unable to open rpmdb"));

    $db;
}

#- register local packages for being installed, keep track of source.
sub register_rpms {
    my ($urpm, @files) = @_;
    my ($start, $id, $error, %requested);

    #- examine each rpm and build the depslist for them using current
    #- depslist and provides environment.
    $start = @{$urpm->{depslist}};
    foreach (@files) {
	/\.(?:rpm|spec)$/ or $error = 1, $urpm->{error}(N("invalid rpm file name [%s]", $_)), next;

	#- if that's an URL, download.
	if (protocol_from_url($_)) {
	    my $basename = basename($_);
	    unlink "$urpm->{cachedir}/partial/$basename";
	    $urpm->{log}(N("retrieving rpm file [%s] ...", $_));
	    if (urpm::download::sync_url($urpm, $_, quiet => 1)) {
		$urpm->{log}(N("...retrieving done"));
		$_ = "$urpm->{cachedir}/partial/$basename";
	    } else {
		$urpm->{error}(N("...retrieving failed: %s", $@));
		unlink "$urpm->{cachedir}/partial/$basename";
		next;
	    }
	} else {
	    -r $_ or $error = 1, $urpm->{error}(N("unable to access rpm file [%s]", $_)), next;
	}

	if (/\.spec$/) {
	    my $pkg = URPM::spec2srcheader($_)
		or $error = 1, $urpm->{error}(N("unable to parse spec file %s [%s]", $_, $!)), next;
	    $id = @{$urpm->{depslist}};
	    $urpm->{depslist}[$id] = $pkg;
	    $pkg->set_id($id); #- sets internal id to the depslist id.
	    $urpm->{source}{$id} = $_;
	} else {
	    ($id) = $urpm->parse_rpm($_);
	    my $pkg = defined $id && $urpm->{depslist}[$id];
	    $pkg or $error = 1, $urpm->{error}(N("unable to register rpm file")), next;
	    $pkg->arch eq 'src' || $pkg->is_arch_compat
		or $error = 1, $urpm->{error}(N("Incompatible architecture for rpm [%s]", $_)), next;
	    $urpm->{source}{$id} = $_;
	}
    }
    $error and $urpm->{fatal}(2, N("error registering local packages"));
    defined $id && $start <= $id and @requested{($start .. $id)} = (1) x ($id-$start+1);

    #- distribute local packages to distant nodes directly in cache of each machine.
    if (@files && $urpm->{parallel_handler}) {
	$urpm->{parallel_handler}->parallel_register_rpms($urpm, @files);
    }

    %requested;
}

#- checks whether the delta RPM represented by $pkg is installable wrt the
#- RPM DB on $root. For this, it extracts the rpm version to which the
#- delta applies from the delta rpm filename itself. So naming conventions
#- do matter :)
sub is_delta_installable {
    my ($urpm, $pkg, $root) = @_;
    $pkg->flag_installed or return 0;
    my $f = $pkg->filename;
    my $n = $pkg->name;
    my ($v_match) = $f =~ /^\Q$n\E-(.*)_.+\.delta\.rpm$/;
    my $db = db_open_or_die($urpm, $root);
    my $v_installed;
    $db->traverse(sub {
	my ($p) = @_;
	$p->name eq $n and $v_installed = $p->version . '-' . $p->release;
    });
    $v_match eq $v_installed;
}

#- extract package that should be installed instead of upgraded,
#- installing instead of upgrading is useful
#- - for inst.list (cf flag disable_obsolete)
#- sources is a hash of id -> source rpm filename.
sub extract_packages_to_install {
    my ($urpm, $sources, $_state) = @_;
    my %inst;

    foreach (keys %$sources) {
	my $pkg = $urpm->{depslist}[$_] or next;
	$pkg->flag_disable_obsolete
	  and $inst{$pkg->id} = delete $sources->{$pkg->id};
    }

    \%inst;
}

#- deprecated
sub install { require urpm::install; &urpm::install::install }

#- deprecated
sub parallel_remove { &urpm::parallel::remove }

#- get reason of update for packages to be updated
#- use all update medias if none given
sub get_updates_description {
    my ($urpm, @update_medias) = @_;
    my %update_descr;
    my ($cur, $section);

    @update_medias or @update_medias = urpm::media::non_ignored_media($urpm, 'update');

    foreach my $medium (@update_medias) {
        # fix not taking into account the last %package token of each descrptions file: '%package dummy'
	foreach (cat_utf8(urpm::media::statedir_descriptions($urpm, $medium)), '%package dummy') {
	    /^%package +(.+)/ and do {
		# fixes not parsing descriptions file when MU adds itself the security source:
		if (exists $cur->{importance} && !member($cur->{importance}, qw(security bugfix))) {
		    $cur->{importance} = 'normal';
		}
		$update_descr{$medium->{name}}{$_} = $cur foreach @{$cur->{pkgs} || []};
		$cur = { pkgs => [ split /\s/, $1 ] };
		$section = 'pkg';
		next;
	    };
	    /^Updated?: +(.+)/ && $section eq 'pkg' and do { $cur->{updated} = $1; next };
	    /^Importance: +(.+)/ && $section eq 'pkg' and do { $cur->{importance} = $1; next };
	    /^(ID|URL): +(.+)/ && $section eq 'pkg' and do { $cur->{$1} = $2; next };
	    /^%(pre|description)/ and do { $section = $1; next };
	    $section  =~ /^(pre|description)\z/ and $cur->{$1} .= $_;
	}
    }
    \%update_descr;
}

sub error_restricted ($) {
    my ($urpm) = @_;
    $urpm->{fatal}(2, N("This operation is forbidden while running in restricted mode"));
}

sub DESTROY {}

1;

__END__

=head1 NAME

urpm - Mandriva perl tools to handle the urpmi database

=head1 DESCRIPTION

C<urpm> is used by urpmi executables to manipulate packages and media
on a Mandriva Linux distribution.

=head2 The urpm class

=over 4

=item urpm->new()

The constructor creates a new urpm object. It's a blessed hash that
contains fields from C<URPM>, and also the following fields:

B<source>: { id => src_rpm_file|spec_file }

B<media>: [ { 
   start => int, end => int, name => string, url => string,
   virtual => bool, media_info_dir => string, with_synthesis => string,
   no-media-info => bool,
   iso => string, downloader => string,
   ignore => bool, update => bool, modified => bool, really_modified => bool,
   unknown_media_info => bool, 
 } ],

=back

=head1 SEE ALSO

The C<URPM> package is used to manipulate at a lower level synthesis and rpm
files.

=head1 COPYRIGHT

Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005 MandrakeSoft SA

Copyright (C) 2005, 2006 Mandriva SA

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

=cut

# ex: set ts=8 sts=4 sw=4 noet:
