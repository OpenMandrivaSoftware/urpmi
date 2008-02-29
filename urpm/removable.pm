package urpm::removable;

# $Id$

use urpm::msg;
use urpm::sys;
use urpm::util;
use urpm 'file_from_local_medium';



sub _file_or_synthesis_dir {
    my ($medium, $o_url) = @_;
    
    urpm::media::_valid_synthesis_dir($medium) && !$o_url ? 
	urpm::media::_synthesis_dir($medium) : 
	file_from_local_medium($medium, $o_url);
}

#- side-effects:
#-   + those of _try_mounting_medium ($medium->{mntpoint})
sub try_mounting_medium {
    my ($urpm, $medium, $o_url) = @_;

    my $rc = _try_mounting_medium($urpm, $medium, $o_url);
    $rc or $urpm->{error}(N("unable to access medium \"%s\".", $medium->{name}));
    $rc;
}

#- side-effects:
#-   + those of urpm::cdrom::try_mounting_cdrom ($urpm->{cdrom_mounted}, $medium->{mntpoint}, "hal_mount")
#-   + those of _try_mounting_local ($urpm->{removable_mounted}, "mount")
sub _try_mounting_medium {
    my ($urpm, $medium, $o_url) = @_;

    if (urpm::is_cdrom_url($medium->{url})) {
	require urpm::cdrom;
	urpm::cdrom::try_mounting_cdrom($urpm, [ { medium => $medium, url => $o_url } ]);
    } else {
	_try_mounting_local($urpm, $medium, $o_url);
    }
}

#- side-effects:
#-   + those of _try_mounting_using_fstab ($urpm->{removable_mounted}, "mount")
#-   + those of _try_mounting_iso ($urpm->{removable_mounted}, "mount")
sub _try_mounting_local {
    my ($urpm, $medium, $o_url) = @_;

    my $dir = _file_or_synthesis_dir($medium, $o_url);
    -e $dir and return 1;

    $medium->{iso} ? _try_mounting_iso($urpm, $dir, $medium->{iso}) : _try_mounting_using_fstab($urpm, $dir);
    -e $dir;
}

#- side-effects: $urpm->{removable_mounted}, "mount"
sub _try_mounting_iso {
    my ($urpm, $dir, $iso) = @_;

    #- note: for isos, we don't parse the fstab because it might not be declared in it.
    #- so we try to remove suffixes from the dir name until the dir exists
    my $mntpoint = urpm::sys::trim_until_d($dir);

    if ($mntpoint) {
	$urpm->{log}(N("mounting %s", $mntpoint));

	#- to mount an iso image, grab the first loop device
	my $loopdev = urpm::sys::first_free_loopdev();
	sys_log("mount iso $mntpoint on $iso");
	$loopdev and system('mount', $iso, $mntpoint, '-t', 'iso9660', '-o', "loop=$loopdev");
	$urpm->{removable_mounted}{$mntpoint} = undef;
    }
    -e $mntpoint;
}

#- side-effects: $urpm->{removable_mounted}, "mount"
sub _try_mounting_using_fstab {
    my ($urpm, $dir) = @_;

    my $mntpoint = _non_mounted_mntpoint($dir);

    if ($mntpoint) {
	$urpm->{log}(N("mounting %s", $mntpoint));
	sys_log("mount $mntpoint");
	system("mount '$mntpoint' 2>/dev/null");
	$urpm->{removable_mounted}{$mntpoint} = undef;
    }
    -e $dir;
}

#- side-effects: $urpm->{removable_mounted}, "umount"
sub try_umounting {
    my ($urpm, $dir) = @_;

    if (my $mntpoint = _mounted_mntpoint($dir)) {
	$urpm->{log}(N("unmounting %s", $mntpoint));
	sys_log("umount $mntpoint");
	system("umount '$mntpoint' 2>/dev/null");
	delete $urpm->{removable_mounted}{$mntpoint};
    }
    ! -e $dir;
}

#- side-effects: none
sub _mounted_mntpoint {
    my ($dir) = @_;
    $dir = reduce_pathname($dir);
    my $entry = urpm::sys::find_a_mntpoint($dir);
    $entry->{mounted} && $entry->{mntpoint};
}
#- side-effects: none
sub _non_mounted_mntpoint {
    my ($dir) = @_;
    $dir = reduce_pathname($dir);
    my $entry = urpm::sys::find_a_mntpoint($dir);
    !$entry->{mounted} && $entry->{mntpoint};
}

#- side-effects: $urpm->{removable_mounted}
#-   + those of try_umounting ($urpm->{removable_mounted}, umount)
sub try_umounting_removables {
    my ($urpm) = @_;
    foreach (keys %{$urpm->{removable_mounted}}) {
	try_umounting($urpm, $_);
    }
    delete $urpm->{removable_mounted};
}

#- side-effects:
#-   + those of try_mounting_non_cdrom ($urpm->{removable_mounted}, "mount")
sub try_mounting_non_cdroms {
    my ($urpm, $list) = @_;

    my $blists = create_blists($urpm->{media}, $list);

    foreach my $blist (grep { urpm::file_from_local_url($_->{medium}{url}) } @$blists) {
	try_mounting_medium($urpm, $blist->{medium}, $blist->{url});
    }
}

#- side-effects: none
sub create_blists {
    my ($media, $list) = @_;

    #- make sure everything is correct on input...
    $media or return;
    @$media == @$list or return;

    my $i;
    [ map { 
	my $list = $list->[$i++];
	my ($url) = values %$list; # first url
	$url ? { medium => $_, list => $list, url => $url } : ();
    } @$media ];
}

sub copy_packages_of_removable_media { 
    require urpm::cdrom;
    &urpm::cdrom::copy_packages_of_removable_media;
}

1;
