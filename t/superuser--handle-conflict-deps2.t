#!/usr/bin/perl

# a1-1 upgrades to a1-2
# b-1 upgrades to b-2 which requires a2
# a2 conflicts with a1
#
# d1-1 upgrades to d1-2
# c-1 upgrades to c-2 which requires d2
# d2 conflicts with d1
#
# nb: d & c is similar to a & b
# (needed to ensure both ordering works)
#
use strict;
use lib '.', 't';
use helper;
use urpm::util;
use Test::More 'no_plan';

need_root_and_prepare();

my $name = 'handle-conflict-deps2';
urpmi_addmedia("$name $::pwd/media/$name");    

# 'c-2','d2-2' is also a valid result; both wanted pkgs can't be installed,
# so urpmi can arbitrarily drop one (after confirming with user, of course)
test(['d1-1', 'c-1'], ['c-2', 'd1-2'], ['c-1', 'd1-2']);

# 'a1-2','b-1' is also a valid result
test(['a1-1', 'b-1'], ['b-2', 'a1-2'], ['b-2', 'a2-2']);


sub test {
    my ($first, $wanted, $result) = @_;

    urpmi("--auto @$first");
    check_installed_fullnames(map { "$_-1" } @$first);

    # test for bug #52153
    system_should_fail("echo n | " . urpmi_cmd() . " @$wanted");
    check_installed_fullnames(map { "$_-1" } @$first);

    urpmi("--auto @$wanted");
    check_installed_fullnames_and_remove(map { "$_-1" } @$result);
}
