#!/usr/bin/perl
use warnings;
use strict;
use Getopt::Std;
use Cwd;
my $wd=cwd();
my %opts=();
if (!@ARGV) {$opts{'h'}=1}
getopts( 'hvCcbim', \%opts );

if ( $opts{'h'} ) {
die "$0 options:
    -c: configure
    -b: build 
    -i: install
    -C: clean, then configure
    -m: MacPorts
";
}
my $mflag=$opts{'m'}?'-fMacPorts':'';
my $v=$opts{'v'}?'--verbose=2':'';
my $build=($opts{'b'} || $opts{'c'} || $opts{'C'})  ?1:0;
my $install=$opts{'i'}?1:0;
if ($install==1) {$build=0};
my $c=$opts{'c'}?1:0;
my $C=$opts{'C'}?1:0;
$c||=$C;
my $os=`uname`;chomp $os;

if (-e "./dist/build/gpcc/gpcc"){
    unlink "./dist/build/gpcc/gpcc";
}

if ($C) {
    system("cabal clean");       
}	
if ($c) {
    print "* Cabal configure\n";
    system("cabal configure $mflag"); 
}
if ($build) {    
    print 'cabal build '.$v."\n";
    system('cabal build '.$mflag.' '.$v);
    
}
if ($install) {
    print "* Cabal install\n";
    print 'cabal install --bindir=../bin'."\n";
    system('cabal install --bindir=../bin');    
}


