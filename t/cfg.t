#!/usr/bin/perl

use Test::More tests => 4;
use MDK::Common;

BEGIN { use_ok 'urpm::cfg' }

my $file = 'testurpmi.cfg';
open my $f, '>', $file or die $!;
print $f (my $cfgtext = <<URPMICFG);
{
  downloader: wget
  fuzzy: no
  verify-rpm: 0
}

update\\ 1 http://foo/bar/ {
  compress: 1
  fuzzy: 1
  keep: yes
  update
  verify-rpm: yes
}

update_2 ftp://foo/bar/ {
  hdlist: hdlist.update2.cz
  ignore
  priority-upgrade: kernel
  synthesis
  with_hdlist: hdlist.update2.cz
}

URPMICFG
close $f;

my $config = urpm::cfg::load_config($file);
ok( ref $config, 'config loaded' );

ok( urpm::cfg::dump_config($file.2, $config), 'config written' );

$cfgtext =~ s/\byes\b/1/g;
$cfgtext =~ s/\bno\b/0/g;
my $cfgtext2 = cat_($file.2);
$cfgtext2 =~ s/# generated.*\n//;
is( $cfgtext, $cfgtext2, 'config is the same' )
    or system qw( diff -u ), $file, $file.2;

END { unlink $file, $file.2 }
