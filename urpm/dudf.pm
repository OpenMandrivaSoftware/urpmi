package urpm::dudf;

# $Id: dudf.pm 639 2009-04-17 14:32:03Z orosello $

our @ISA = qw();
use strict;
use Exporter;
use URPM;
use urpm;
use urpm::msg;
use urpm::util;
use Cwd;
use IO::File;
use locale;
use POSIX qw(locale_h strtod);
use POSIX qw(strftime);
use File::Path;
use Compress::Zlib;
use XML::Writer;
use Data::UUID;
use Digest::MD5 'md5_hex';
use dudfrpmstatus;

#- Timeout for curl connection and wget operations
our $CONNECT_TIMEOUT = 60; #-  (in seconds)

use fields qw(
    access_url
    distribution_codename
    distribution_description
    distribution_name
    distribution_release
    dudf_dir
    dudf_file
    dudf_filename
    dudf_time
    dudf_uid
    dudf_urpm
    exit_code
    exit_msg
    force_dudf
    installer_name
    installer_version
    log_file
    log_msg
    metainstaller_name
    metainstaller_version
    package_universe_synthesis
    packages_removed
    packages_upgraded
    pkgs_toinstall
    pkgs_user
    pkgs_user_install
    pkgs_user_upgrade
    version
    );

my @package_status;

BEGIN {}

(our $VERSION) = q($Revision: 246 $) =~ /(\d+)/;

sub dudf_exit {
    my ($self, $exit_code, $o_exit_msg) = @_;
    $self->set_exit_code($exit_code);
    if ($o_exit_msg) {
        $self->set_exit_msg($o_exit_msg);
    }
    
    my $noexpr = N("Nn");
    my $msg = N("A problem occurred. You can contribute to the improvement of the Mandriva upgrade\nprocess by uploading an automatically generated report to a Mandriva server.\nNo personal information will be transmitted. More information is available at\nhttp://doc4.mandriva.org/bin/dudf/.\n");
    $msg .= N("Do you want to generate and upload a report?");

    if ( (!${$self->{dudf_urpm}}->{options}{auto}) &&
         ( ($self->{exit_code} > 9) ||
           (!$self->{exit_code} && $self->{force_dudf}) ) ) {
       
      if ($self->{force_dudf} || message_input_($msg . N(" (Y/n) "), boolean => 1) !~ /[$noexpr]/) {           
      
        $self->upload_synthesis_files;
        $self->write_dudf;
        $self->upload_dudf;
        
      }
        
    }
    exit($exit_code);
}

sub get_distribution {
    my ($self) = @_;

    my $handle = new IO::File;
    if ($handle->open("</etc/lsb-release")) {
        while (<$handle>) {
                if (m/DISTRIB_ID=/i)            { s/.*=//; s/\n//; $self->{distribution_name} = $_ }
                if (m/DISTRIB_RELEASE=/i)       { s/.*=//; s/\n//; $self->{distribution_release} = $_ }
                if (m/DISTRIB_CODENAME=/i)      { s/.*=//; s/\n//; $self->{distribution_codename} = $_ }
                if (m/DISTRIB_DESCRIPTION=/i)   {   s/.*=//; s///g; s/\n//; $self->{distribution_description} = $_ }
        }
        $handle->close;
    }
}

sub check_package {
    my ($urpm, $pkg) = @_;
    my $db = urpm::db_open_or_die_($urpm);
    my @l;
    $db->traverse_tag("name", [ $pkg ], sub {
                                                my ($p) = @_;
                                                $p->pack_header;
                                                push(@l, $p);
                                              });
    \@l;
}

# Find packages selected to be removed due to obsoletes and store them into @{$self->{packages_removed}}
# or due to upgrade or conflict and store them into @{$self->{packages_upgraded}}
sub check_removed_upgraded {
    my ($self, $state) = @_;
    my $urpm = ${$self->{dudf_urpm}};
    my $t = $state->{rejected};

    foreach my $pkg (keys %$t) {
        my $v = $t->{$pkg};
        if ($v->{obsoleted} == 1) {
            $pkg =~ s/-.*//;
            my $p = check_package($urpm,$pkg);
            push(@{$self->{packages_removed}}, $p);
        }
        if ($v->{removed} == 1) {
            $pkg =~ s/-.*//;
            my $p = check_package($urpm,$pkg);
            push(@{$self->{packages_upgraded}}, $p);
        }
    }        
}

sub get_package_status_ {
    my ($ps) = @_;
    $ps->pack_header;
    push(@package_status, $ps);
}

# Store list of installed packages
sub get_package_status {
    my ($self) = @_;
    my $db = urpm::db_open_or_die_(${$self->{dudf_urpm}});
    $db->traverse(\&get_package_status_);
}

# Store list of synthesis files to parse
sub get_package_universe {
    my ($self) = @_;
    my $urpm = ${$self->{dudf_urpm}};

    @{$self->{package_universe_synthesis}} = grep { !$_->{ignore} } @{$urpm->{media}};
}

sub get_synthesis_md5 {
    my ($self, $file) = @_;
    open(SYNTFILE, $file) or die "Can't open '$file': $!";
    binmode(SYNTFILE);
    my $md5sum = Digest::MD5->new->addfile(*SYNTFILE)->hexdigest;
    return $md5sum;
}

# Parse a synthesis file
sub get_synthesis {
    my ($self, $file, $doc) = @_;
    my $buffer;

    my $gz = gzopen($file, "rb");
# or die "Cannot open $file: $gzerrno\n" ;

    $doc->characters($buffer) 
        while $gz->gzread($buffer) > 0;
#    die "Error reading from $file: $gzerrno\n" 
#        if my $gzerrno != Z_STREAM_END ;

    $gz->gzclose;
}

sub new {
    my ($class, $urpm, $action, $force_dudf) = @_;
    my $self = {
        dudf_urpm => $urpm,
        action => $action,
        force_dudf => $force_dudf,
        dudf_file => undef,
        exit_code => 0,
        metainstaller_name => $0,
        metainstaller_version => $urpm::VERSION,
        version => "1.0",
        dudf_time => undef
    };

    my $base_url = "http://doc4.mandriva.org:8087/dudf";
    $self->{access_url} = "http://doc4.mandriva.org/bin/view/dudf/instance";
    $self->{upload_url} = $base_url . "/upload";
    $self->{synthesis_base_url} = "http://doc4.mandriva.org:8087/synthesis/";
    $self->{metainstaller_name} =~ s/.*\///;

    ${$self->{dudf_urpm}}->{fatal} = sub { printf STDERR "%s\n", $_[1]; $self->set_exit_msg($_[1]);  $self->set_exit_code($_[0]); $self->dudf_exit(10); };
    ${$self->{dudf_urpm}}->{error} = sub { printf STDERR "%s\n", $self->set_exit_msg($_[0]); $_[0] };
    ${$self->{dudf_urpm}}->{log}   = sub { $self->add_log_msg($_[0]); };

    $urpm = ${$self->{dudf_urpm}};
    $self->{dudf_dir} = $urpm->{cachedir} . "/dudf";
    $self->{log_file} = $self->{dudf_dir} . "/dudf_uploads.log";
    if (!-d $self->{dudf_dir}) {
        mkpath($self->{dudf_dir});
    }

    # If there is no log file, we create the default content here
    if (! -f $self->{log_file})  {
	output_safe($self->{log_file}, 
                    N("# Here are logs of your DUDF uploads.\n# Line format is : <date time of generation> <uid>\n# You can use uids to see the content of your uploads at this url :\n# http://dudf.forge.mandriva.com/\n"));
    }
    my $ug = new Data::UUID;
    $self->{dudf_uid} = $ug->to_string($ug->create_str);

    bless($self,$class);
    return $self;
}

sub set_error_msg {
    my ($self,$m) = @_;
    $self->{exit_msg} .= $m;
}

sub get_error_msg {
    my ($self) = @_;
    return $self->{exit_msg} if defined($self->{exit_msg});
    return '';
}

sub set_exit_msg {
    my ($self, $m) = @_;
    $self->{exit_msg} .= $m;
}

sub add_log_msg {
    my ($self, $m) = @_;
    $self->{log_msg} .= $m . "\n";
}

sub get_log_msg {
    my ($self) = @_;
    return $self->{log_msg} if defined($self->{log_msg});
    return '';
}

# store the exit code
sub set_exit_code {
    my ($self, $exit_code) = @_;
    $self->{exit_code} = $exit_code;
}

# Store the list of packages the user wants to install (given to urpmi)
sub store_userpkgs {
    my ($self, @pkgs) = @_;
    @{$self->{pkgs_user}} = @pkgs;
}

# Store a list of packages selected by urpmi to install
sub store_toinstall {
    my ($self, @pkgs) = @_;
    @{$self->{pkgs_toinstall}} = @pkgs;
}

#upload dudf data to server
sub upload_dudf {
    -x "/usr/bin/curl" or do { print N("curl is missing, cannot upload DUDF file.\n"); return };
    my ($self, $options) = @_;

    print N("Compressing DUDF data... ");
    # gzip the file to upload
    open(FILE, $self->{dudf_file}) or do { print N("NOT OK\n"); return };
    my $gz = gzopen($self->{dudf_file} . ".gz", "wb") or do { print N("NOT OK\n"); return };
    $gz->gzsetparams(Z_BEST_COMPRESSION, Z_DEFAULT_STRATEGY);
   
    while (<FILE>) {
        $gz->gzwrite($_);
    }
    $gz->gzclose;
    close(FILE);
    print N("OK\n");

    print N("Uploading DUDF data:\n");
    my (@ftp_files, @other_files);
    push @other_files, $self->{dudf_filename};
    my @l = (@ftp_files, @other_files);
    my $cmd = join(" ", map { "'$_'" } "/usr/bin/curl",
        "-q", # don't read .curlrc; some toggle options might interfer
        ($options->{proxy} ? urpm::download::set_proxy({ type => "curl", proxy => $options->{proxy} }) : ()),
        ($options->{retry} ? ('--retry', $options->{retry}) : ()),
        "--stderr", "-", # redirect everything to stdout
        "--connect-timeout", $CONNECT_TIMEOUT,
#                "-s",
        "-f", 
        "--anyauth",
        (defined $options->{'curl-options'} ? split /\s+/, $options->{'curl-options'} : ()),
        "-Ffile=@" . $self->{dudf_file} . ".gz",
        "-Fid=" . $self->{dudf_uid},
        $self->{upload_url},
        );
    print N("\nCommand:");
    print $cmd;
    urpm::download::_curl_action($cmd, $options, @l, 1);
    unlink $self->{dudf_file} . ".gz";
    unlink $self->{dudf_file};
    print N("\nYou can see your DUDF report at the following URL :\n\t");
    print $self->{access_url} . "?uid=" . $self->{dudf_uid} . "\n";
    append_to_file($self->{log_file}, $self->{dudf_time} . "\t" . $self->{dudf_uid} . "\n");
    print N("You can access a log of your uploads in\n\t") . $self->{log_file} . "\n";
}

sub upload_synthesis_file {
    -x "/usr/bin/curl" or do { print N("curl is missing, cannot upload DUDF file.\n"); return };
    my ($self, $options, $media, $filename, $md5sum) = @_;

    print N("Testing synthesis file presence ...");
    my $cmd_synthesis_check = join(" ", map { "'$_'" } "/usr/bin/curl",
        "-q", # don't read .curlrc; some toggle options might interfer
        ($options->{proxy} ? urpm::download::set_proxy({ type => "curl", proxy => $options->{proxy} }) : ()),
        ($options->{retry} ? ('--retry', $options->{retry}) : ()),
        "--stderr", "-", # redirect everything to stdout
        "--connect-timeout", $CONNECT_TIMEOUT,
        "-s",
        "-f", 
        "--anyauth",
        (defined $options->{'curl-options'} ? split /\s+/, $options->{'curl-options'} : ()),
        $self->{synthesis_base_url} . "$md5sum/exists",
    );

    #print N("\nCommand:");
    #print $cmd_synthesis_check;
    my $check_result = `$cmd_synthesis_check`;

    if ($check_result eq 1) {
        print N("\nThe synthesis file (media '%s', %s) is already available on the Mandriva DUDF server.\n", $media, $md5sum);
    } else {
        print N("\nUploading synthesis file ...");
        my $cmd_synthesis_upload = join(" ", map { "'$_'" } "/usr/bin/curl",
            "-q", # don't read .curlrc; some toggle options might interfer
            ($options->{proxy} ? urpm::download::set_proxy({ type => "curl", proxy => $options->{proxy} }) : ()),
            ($options->{retry} ? ('--retry', $options->{retry}) : ()),
            "--stderr", "-", # redirect everything to stdout
            "--connect-timeout", $CONNECT_TIMEOUT,
            #                "-s",
            "-f", 
            "--anyauth",
            (defined $options->{'curl-options'} ? split /\s+/, $options->{'curl-options'} : ()),
            "-Ffile=@" . $filename,
            "-Fmd5=" . $md5sum,
            $self->{synthesis_base_url} . "upload",
        );
        #print N("\nCommand:");
        #print $cmd_synthesis_upload;
        urpm::download::_curl_action($cmd_synthesis_upload, $options, (), 1);
        print N("\nSynthesis successfully uploaded. It can be downloaded from") . " " . $self->{synthesis_base_url} . $md5sum . "/download\n";
    }
}

sub upload_synthesis_files {
    my ($self) = @_;

    $self->get_package_status;
    $self->get_package_universe;
    $self->compute_pkgs_user;

    foreach my $media (@{$self->{package_universe_synthesis}}) {
        my $options = ();
        my $file = $media->{name};
        my $url = $media->{url};
        my $filename = urpm::media::any_synthesis(${$self->{dudf_urpm}}, $media);
        my $md5sum = $self->get_synthesis_md5($filename);

        $self->upload_synthesis_file($options, $file, $filename, $md5sum);
    }
}

sub compute_pkgs_user {
    my ($self) = @_;
    my $in;

    foreach my $p (@{$self->{pkgs_user}}) {
        $in = 0;
        foreach my $pk (@package_status) {
            # packages installed by urpmi are removed from the list 
            if (@{$self->{pkgs_toinstall}}) {
                foreach my $pkg (@{$self->{pkgs_toinstall}}) {
                    if ($p eq $pk->name) {
                        if (($pkg->name ne $pk->name ||
                             $pkg->version ne $pk->version ||
                             $pkg->arch ne $pk->arch ||
                             $pkg->release ne $pk->release ||
                             $pkg->epoch ne $pk->epoch)) {
                            $in = 1;
                            if (!grep($p, @{$self->{pkgs_user_upgrade}})) {
                                push(@{$self->{pkgs_user_upgrade}}, $p);
                            }
                        }
                    }                    
                }
            }
        }
        if (($in == 0) && (!grep($p, @{$self->{pkgs_user_install}}))) {
            push(@{$self->{pkgs_user_install}}, $p);
        }
    }
}

# Generate DUDF data
sub write_dudf {
    my ($self) = @_;
    
	print N("\nGenerating DUDF... ");
	
	my $urpm = ${$self->{dudf_urpm}};
	my $choices = $urpm->{_dudf};

	my $GlobalDUDF = dudfrpmstatus::GlobalDudf->new();

	$GlobalDUDF->setUrpmiConfig($urpm->{config});
	$GlobalDUDF->setUrpmiMediaCfgDir($urpm->{mediacfgdir});
	$GlobalDUDF->setUrpmiSkiplist($urpm->{skiplist});
	$GlobalDUDF->setUrpmiInstlist($urpm->{instlist});
	$GlobalDUDF->setUrpmiPreferList($urpm->{prefer_list});
	$GlobalDUDF->setUrpmiPreferVendorlist($urpm->{prefer_vendor_list});
	$GlobalDUDF->setUrpmiPrivateNetrc($urpm->{private_netrc});
	$GlobalDUDF->setUrpmiStateDir($urpm->{statedir});
	$GlobalDUDF->setUrpmiCacheDir($urpm->{cachedir});
	$GlobalDUDF->setUrpmiRoot($urpm->{root});
	$GlobalDUDF->setRpmStateDir("/var/lib/rpm/");
	$GlobalDUDF->urpmiEnvSet();

	$GlobalDUDF->SetDistro();
	$GlobalDUDF->AddMsgError($self->get_error_msg());
	$GlobalDUDF->AddMsgLog($self->get_log_msg());
	$GlobalDUDF->setUrpmiRequest($self->{action});
	$GlobalDUDF->setUrpmiRequestStatus( ($self->{exit_code} > 9 ? 0 : 1) );
	$GlobalDUDF->setUrpmiForceDudf($self->{force_dudf});
	$GlobalDUDF->setMetaInstallerName($self->{metainstaller_name});
	$GlobalDUDF->setMetaInstallerVersion($self->{metainstaller_version});

	foreach my $h (@{$choices}) {
		my $depname = $h->{virtualpkgname};
		my $list = $h->{list};
		$list =~ s/^<//g;
		$list =~ s/>$//g;
		my $chosen = $h->{chosen};
		my $prefered = $h->{prefered};
		my $properties = $h->{properties};

		$GlobalDUDF->setVirtualPackageNameDependency($depname);
		$GlobalDUDF->setVirtualPackageNameSelection($list);

		map {
			my $fname = "" . $_->fullname;
			$GlobalDUDF->pushVirtualPackageNameChoice($fname);
		} @$chosen;
		
		$GlobalDUDF->commitVirtualPackageSelection();
	}

	$GlobalDUDF->DumpToFile();

	$self->{dudf_time} = strftime("%Y-%m-%dT%H:%M:%S%Z", localtime());
	$self->{dudf_filename} = "dudf_" . $GlobalDUDF->GetFilenameFingerprint() . ".xml";
	$self->{dudf_file} = $self->{dudf_dir} . "/" . $self->{dudf_filename};

	print N("OK\n");

}

1;

__END__
