#!/usr/bin/perl

# a-1 requires b-1
# a-2 requires b-2
#
# c requires d
# d1-1 provides d, but not d1-2
# d2-2 provides d, but not d2-1
#
# e-2 conflicts with f-1
#
# g-2 conflicts with h-1
#
use strict;
use lib '.', 't';
use helper;
use urpm::util;
use Test::More 'no_plan';

need_root_and_prepare();

my $name = 'split-transactions--promote';
urpmi_addmedia("$name-1 $::pwd/media/$name-1");    
urpmi_addmedia("$name-2 $::pwd/media/$name-2");

test('--split-length 0');
test('--split-level 1');

sub test {
    my ($split) = @_;

    test_ab("$split --auto-select");

    #- below need the promotion of "a-2" (upgraded from "a-1") to work
    test_ab("$split b");

    #- below need the promotion of "d2" (new installed package) to work
    test_cd("$split d1");

    #- below need the promotion of "f-2" (upgraded from "f-1") to work
    test_ef("$split e");

    #- WARNING: below would need the promotion of "h-2" (upgraded from "e-1")
    test_gh("$split g");
}

sub test_ab {
    my ($para) = @_;

    urpmi("--media $name-1 --auto a b");
    check_installed_names('a', 'b');

    urpmi("--media $name-2 --auto $para");
    check_installed_fullnames_and_remove('a-2-1', 'b-2-1');
}

sub test_cd {
    my ($para) = @_;

    urpmi("--media $name-1 --auto c");
    check_installed_names('c', 'd1');

    urpmi("--media $name-2 --auto $para");
    check_installed_fullnames_and_remove('c-1-1', 'd1-2-1', 'd2-2-1');
}

sub test_ef {
    my ($para) = @_;

    urpmi("--media $name-1 --auto e f");
    check_installed_names('f', 'e');

    urpmi("--media $name-2 --auto $para");
    check_installed_fullnames_and_remove('e-2-1', 'f-2-1');
}

sub test_gh {
    my ($para) = @_;

    urpmi("--media $name-1 --auto g h");
    check_installed_names('g', 'h');

    urpmi("--media $name-2 --auto $para");
    check_installed_fullnames_and_remove('g-2-1'); # WARNING
}