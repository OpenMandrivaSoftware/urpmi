#!/usr/bin/perl

use strict;
use lib '.', 't';
use helper;
use Test::More 'no_plan';
use Cwd;

chdir 't' if -d 't';
system('rm -rf tmp media');
foreach (qw(media tmp tmp/BUILD tmp/RPMS tmp/RPMS/noarch tmp/SRPMS)) {
    mkdir $_;
}
my $genhdlist2 = 'genhdlist2 --xml-info';

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
	system_("$genhdlist2 media/${name}_no_subdir");
	symlink "${name}_nohdlist", "media/${name} nohdlist";
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
    system_("$genhdlist2 media/${name}_no_subdir");
    genhdlist_std($name);
}

mkdir 'media/reconfig';
system_("cp -r data/reconfig.urpmi media/reconfig");

mkdir 'media/media_info';
system_("cp -r data/media.cfg media/media_info");
system_('gendistrib -s .');

sub genhdlist_std {
    my ($medium_name) = @_;
    system_("$genhdlist2 media/$medium_name");
}

sub rpmbuild {
    my ($spec, $o_medium_name) = @_;

    my $dir = getcwd();
    my ($target) = $spec =~ m!-(i586|x86_64)\.spec$!;
    system_("rpmbuild --undefine _build_pkgcheck --undefine _build_pkgcheck_set --undefine _build_pkgcheck_srpm --undefine _nonzero_exit_pkgcheck_terminate_build --define 'disttag mdk' --define 'distepoch 2013.0' --quiet --define 'rpm_version %(rpm -q --queryformat \"%{VERSION}\" rpm|sed -e \"s/\\\\.//g\")' --define '_topdir $dir/tmp' --define '_tmppath $dir/tmp' --define '_rpmdir $dir/tmp/RPMS' --define '_build_name_fmt %%{ARCH}/%{___NVRA}.rpm' --define '_rpmfilename %{_build_name_fmt}' -bb --clean --nodeps ".($target ? "--target $target" : "")." $spec");

    my ($name) = $spec =~ m!([^/]*)\.spec$!;

    my $medium_name = $o_medium_name || $name;
    mkdir "media/$medium_name";
    system_("mv tmp/RPMS/*/*.rpm media/$medium_name");

    $medium_name;
}

sub rpmbuild_srpm {
    my ($spec) = @_;

    my $dir = getcwd();
    system_("rpmbuild --undefine _build_pkgcheck --undefine _build_pkgcheck_set --undefine _build_pkgcheck_srpm --undefine _nonzero_exit_pkgcheck_terminate_build --define 'disttag mdk' --define 'distepoch 2013.0' --quiet --define '_topdir $dir/tmp' --define '_rpmdir $dir/tmp/RPMS' --define '_build_name_fmt %%{ARCH}/%{___NVRA}.rpm' --define '_rpmfilename %{_build_name_fmt}' -bs --clean --nodeps $spec");

    my ($name) = $spec =~ m!([^/]*)\.spec$!;

    my $medium_name = "SRPMS-$name";
    mkdir "media/$medium_name";
    system_("mv tmp/SRPMS/*.rpm media/$medium_name");

    $medium_name;
}
