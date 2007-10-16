#!/usr/bin/perl

use strict;
use lib '.', 't';
use helper;
use Test::More 'no_plan';

chdir 't' if -d 't';
system('rm -rf tmp media');
foreach (qw(media tmp tmp/BUILD tmp/RPMS tmp/RPMS/noarch tmp/SRPMS)) {
    mkdir $_;
}

# locally build test rpms

foreach my $dir (grep { -d $_ } glob("data/SPECS/*")) {
    my ($medium_name) = $dir =~ m!([^/]*)$!;
    rpmbuild($_, $medium_name) foreach glob("$dir/*.spec");
    genhdlist_std($medium_name);
}

foreach my $spec (glob("data/SPECS/*.spec")) {
    my $name = rpmbuild($spec);

    if ($name eq 'various') {
	system_("cp -r media/$name media/${name}_nohdlist");
	system_("cp -r media/$name media/${name}_no_subdir");
	system_("genhdlist --dest media/${name}_no_subdir");
	symlink "${name}_nohdlist", "media/${name} nohdlist";
	symlink "${name}", "media/${name}_bis";
    }
    genhdlist_std($name);
}
foreach my $spec (glob("data/SPECS/srpm*.spec")) {
    my $name = rpmbuild_srpm($spec);
    genhdlist_std($name);
}

{
    my $name = 'rpm-v3';
    system_("cp -r data/$name media");
    system_("cp -r media/$name media/${name}_nohdlist");
    system_("cp -r media/$name media/${name}_no_subdir");
    system_("genhdlist --dest media/${name}_no_subdir");
    genhdlist_std($name);
}

mkdir 'media/media_info';
system_("cp -r data/media.cfg media/media_info");
system_('gendistrib -s .');

sub genhdlist_std {
    my ($medium_name) = @_;
    system_("genhdlist --subdir media/$medium_name/media_info media/$medium_name");
}

sub rpmbuild {
    my ($spec, $o_medium_name) = @_;

    system_("rpmbuild --quiet --define '_topdir tmp' -bb --clean --nodeps $spec");

    my ($name) = $spec =~ m!([^/]*)\.spec$!;

    my $medium_name = $o_medium_name || $name;
    mkdir "media/$medium_name";
    system_("mv tmp/RPMS/*/*.rpm media/$medium_name");

    $medium_name;
}

sub rpmbuild_srpm {
    my ($spec) = @_;

    system_("rpmbuild --quiet --define '_topdir tmp' -bs --clean --nodeps $spec");

    my ($name) = $spec =~ m!([^/]*)\.spec$!;

    my $medium_name = "SRPMS-$name";
    mkdir "media/$medium_name";
    system_("mv tmp/SRPMS/*.rpm media/$medium_name");

    $medium_name;
}
