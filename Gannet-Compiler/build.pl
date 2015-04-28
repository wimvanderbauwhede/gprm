#!/usr/bin/perl
use warnings;
use strict;
use Getopt::Std;
use Cwd;
my $wd=cwd();
my %opts=();
if (!@ARGV) {$opts{'h'}=1}
getopts( 'hvCcbi', \%opts );

if ( $opts{'h'} ) {
die "$0 options:
    -c: configure
    -b: build 
    -i: install
    -C: clean, then configure
";
}
my $v=$opts{'v'}?'--verbose=2':'';
my $build=($opts{'b'} || $opts{'c'} || $opts{'C'})  ?1:0;
my $install=$opts{'i'}?1:0;
if ($install==1) {$build=0};
my $c=$opts{'c'}?1:0;
my $C=$opts{'C'}?1:0;
$c||=$C;
my $os=`uname`;chomp $os;
#rename 'Gannet.cabal.OFF', 'Gannet.cabal';
#rename 'Gannet-Compiler.cabal', 'Gannet-Compiler.cabal.OFF';
#my $NEWflag=$opts{'N'}?'-fNEW':'';
my $W=64;
#my $wflag = '-fW'.$W; 

if (-e "./dist/build/gannetc/gannetc"){
    unlink "./dist/build/gannetc/gannetc";
}

if ($C) {
    system("cabal clean");       
}	
if ($c) {
    print "* Cabal configure\n";#, $W-bit, flags: $NEWflag\n";
    system("cabal configure"); # $wflag $NEWflag");
}
if ($build) {    
#    print "* Cabal build\n";
#    print 'cabal build --ghc-options="-cpp -O -DWORDSZ='.$W.'" '.$v."\n";
#    system('cabal build --ghc-options="-cpp -O -DWORDSZ='.$W.'" '.$v);
    print 'cabal build '.$v."\n";
    system('cabal build '.$v);
    
}
if ($install) {
    print "* Cabal install\n";
#print 'cabal install -v2 --ghc-options="-cpp -DWORDSZ='.$W.'" --bindir=../bin'."\n";
#    print 'cabal install --ghc-options="-cpp -O -DWORDSZ='.$W.'" --bindir=../bin'."\n";
#    system('cabal install --ghc-options="-cpp -O -DWORDSZ='.$W.'" --bindir=../bin');
    print 'cabal install --bindir=../bin'."\n";
    system('cabal install --bindir=../bin');    
    system("cp ../bin/gannetc ../bin/gannetc$W");
}

#rename 'Gannet-Compiler.cabal.OFF', 'Gannet-Compiler.cabal';
#rename 'Gannet.cabal', 'Gannet.cabal.OFF';

