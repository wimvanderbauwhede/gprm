#!/usr/bin/env perl
use 5.012;
use warnings;
use strict;

# build script for GPRM
use Config;
use Cwd;
#use Getopt::Std;
use Getopt::Long;
use YAML::Syck;
use Data::Dumper;
use WrapperGenerator;

my $gannet_dir = $ENV{GPRM_DIR};
my $wd=cwd();
my $platform = 'darwin';
my $os=`uname`;
chomp $os;
if ($os=~/Linux/) {
    $platform='linux';
}
    
#my %opts;
#getopts( 'ihvwNt:Y:n:cX:dbgSse', \%opts );
my $help=0;
my $init= 0;

my $warn= 0;
my $verbose= 0;
my $build=1;
my $generate=1;
my $clean=0;
my $debug=0;
my $steal= 0;


my $nthreads=4; # Ad hoc!
my $no_pthreads=0;
my $cross_compile='';
my $build_exe=0;
my $build_shared_lib=0;
my $count_cycles=0;

my $task_path= "$wd/src/GPRM/Task";
my $task_name = '';
my $ymlfile ='';
my $stateful_kernel=0;
my $kernel_lock=0;
my $no_args=0;
GetOptions (
    'help' => \$help,
    'init' => \$init,
    'nthreads=i' => \$nthreads,   
    'task=s'   => \$task_name,   
    'stateful' => \$stateful_kernel,
    'locking' => \$kernel_lock,
    'steal' =>\$steal,
         
    'warn' => \$warn,
    'verbose'  => \$verbose,
    'debug'  => \$debug,
    'clean' =>\$clean,
    
    'yml=s' => \$ymlfile,
    'xc=s' => \$cross_compile,
    
    'generate' => $generate,
    'build' =>\$build,
    'exe' =>\$build_exe,
    'shlib' => \$build_shared_lib,
    'singlethread' => \$no_pthreads,
    'cycles' => \$count_cycles,
) or do {$no_args=1};

if ( $help or $no_args) {
	die "
    Gannet build script for GPRM
    gprm --init,-i:    Init, create the directory structure and source file skeleton for GPRM
    gprm --task,-t <your task> --nthreads,-n <number of hreads> 
        --task,-t: Name of the toplevel GPRM Task class which should reside in src/GPRM/Task/  
        --nthreads,-n: Number of hardware threads to be used (4 by default) 
    gprm --clean,-c: clean
    
    Kernels are assumed to be stateless. To use stateful kernels, use the option --stateful
    To have a lock around each kernel method access, use --locking 
    
    Following options are intended for advanced use
        
    --yml,-Y YAML-file.yml: SBA config file to use (normally generated from the Task header)    
    -xc,-X: cross-compile for Linux on Tilera or MIC (specify platform name)
    --steal: use task stealing


    Following options are intended for use in development/testing/debugging
    --warn,-w: warnings 
    --verbose,-v: verbose
    --debug,-d: debug 
    --singlethread: DO NOT use POSIX threads, for single-threaded debugging        
    \n";
}
=pod Extra Options

    -g: generate SystemConfiguration.h from YAML-file, don't build
    -b: build only, don't generate
    -e: compile as binary executable
    -s: compile as a shared library (default is static library)    
    
=cut    

#my $wd=cwd();
if ($init) {    
    system("tar -zxvf $gannet_dir/examples/gprm-project-skeleton.tgz");
}

if ($init and not -e "$gannet_dir/bin/gpcc-$platform") {       
    chdir "$gannet_dir/GPC";
    system('cabal configure');
    system('cabal build');
    system('cabal install --bindir=../bin');
    chdir $wd;    
}
if ($init and not -e "$gannet_dir/bin/gannetc-$platform") {    
    chdir "$gannet_dir/Gannet-Compiler";
    system('./build.pl -C -i');
    chdir $wd;
}    
if ($init) {    
    die "Done init. 
Add your own sources in src/ and your GPRM kernels and tasks in src/GPRM/Kernel and src/GPRM/Task respectively.
You can compile your project without GPRM for testing, see the SConstruct file in src/
To compile with GPRM, run

  gprm -t <YourTask> -n <NThreads>
  
This will build the GPRM runtime and also create a SConstruct.gprm file to build your code with GPRM.
";      
}




$generate= $build?($generate?1:0):1;
$build= $generate?($build?1:0):1;
my $scons_c=$clean?'-c':'';

my $scons_ext='';#.local';

my $scons_nogen='nogen=1';
my $scons_v=$verbose?'v=1':'';
my $scons_w=$warn?'w=1':'';
my $scons_d=$debug?'dbg=1':'';

my $scons_pthreads=$no_pthreads?'':'pthreads=1';
my $scons_xc=($cross_compile ne '')?'xc='.$cross_compile:'';
my $scons_lib = $build_exe?'':'lib=1';
my $scons_shlib=$build_shared_lib? ($build_exe ? ''  : 'shlib=1') : '';

# -C: count cycles
my $scons_cycles=$count_cycles?'cycles=1':'';

my $scons_wordsz='wordsz=64';
my $scons_new='new=1';
my $scons_dyn='dyn=1';
my $scons_vm='vm=1';
my $scons_svm='';
my $scons_wd='wd='.$wd;
my $scons_stateful=$stateful_kernel ? 'stateful=1' : '';
my $scons_kernel_lock=$kernel_lock ? 'locking=1' : '';

my $scons_steal=$steal ? 'steal=1' : '';
my $opts={};
if ($stateful_kernel) { $opts->{'stateful'}=1 }
if ($steal) { $opts->{'steal'}=1 }
if ($no_pthreads) { $opts->{'nothreads'}=1 }
if ($kernel_lock) {$opts->{'locking'}=1 }

if ($task_name eq '') {
    die "Please specify the task name with -t\n";
} elsif ($task_name=~/\//) { 
    $task_path= $task_name;
    $task_path=~s/\.\w+\s*$//; # strip extension
    $task_name= pop( @{split(/\//,$task_path)}); # split on / and take last word
    $task_path=~s/\/\w+$//; # remove last word + leading / from path
}
$ymlfile ="$task_path/$task_name.yml";

if (!$clean) {
    WrapperGenerator::generate_yml($task_name, $task_path, $nthreads);
}


my $ymlpath=$ymlfile;
if ($ymlfile!~/^\//) {
	$ymlpath="$wd/$ymlfile";
}
my $scons_sclib='';
my $sclib='';
#say $ymlpath;
my $config_href = YAML::Syck::LoadFile($ymlpath);
#my $yaml = YAML::Tiny->read($ymlpath);
#my $config_href =$yaml->[0];

my %config = %{$config_href};
my @sclibs=@{ $config{'System'}{'Libraries'} };
$sclib=join(',',@sclibs);
$scons_sclib='sclib='.$sclib;

my $cxx_gen_source_path="$wd/gensrc";
my $cxx_source_path="$gannet_dir/GPRM/SBA";
#my $cxx_build_path="$gannet_dir/GPRM/build";
my $cxx_build_path="$wd";
my $gprm_lib_path="$wd/lib";

my $run_scons_str="GPRM_YML_CONFIG=$ymlpath scons $scons_c $scons_new $scons_sclib $scons_v $scons_w $scons_d $scons_cycles $scons_dyn $scons_vm $scons_pthreads $scons_stateful $scons_kernel_lock $scons_steal $scons_wordsz $scons_nogen $scons_wd $scons_lib $scons_shlib $scons_xc -f $gannet_dir/GPRM/build/SConstruct$scons_ext";
$run_scons_str=~s/\s+/ /g;

#$wd="$gannet_dir/GPRM/build";
if ($clean) { # CLEAN!
# Clean GPRM build
    system('scons -f SConstruct.gprm -c'); 
	say "$run_scons_str" if $verbose;
	system("$run_scons_str");
	say "gprm: Cleaning gensrc..." if $verbose;
	for my $file (qw(SystemConfiguration.h Services.h Services.cc SelectWrapper.cc),"$task_name.cc","$task_name.o") {
		if (-e "./gensrc/$file") {
			say "rm ./gensrc/$file" if $verbose;
			unlink "./gensrc/$file";
		}
	}
	for my $file (@sclibs) {
		next if $file eq 'CoreServices';
		if (-e "./gensrc/$file.yml") {
			say "rm ./gensrc/$file.yml" if $verbose;
			unlink "./gensrc/$file.yml";
		}
	}
	if (-e './bin/gannetvm64') {
		say 'rm ./bin/gannetvm64' if $verbose;
		unlink  './bin/gannetvm64';
	}
	if (-e './lib/libgannet.a') {
		say 'rm ./lib/libgannet.a' if $verbose;
		unlink  './lib/libgannet.a';
	}
	if (-e 'GprmConfig.py') {
	    unlink 'GprmConfig.py';
	    unlink 'GprmConfig.pyc';
	}
die "Done cleaning\n";
} else { # GENERATE
	if ($generate) {
# 0. Generate .yml library configuration from kernel class(es)
#    and generate the wrapper function(s)		
		my $changed = `scons -s -f $gannet_dir/GPRM/build/SConstruct.CheckChange.py SRC=$ymlpath`;
		chomp $changed;
        my $is_core=0;
        my $nclasses=@sclibs;
		for my $class (@sclibs) {
            if ($class eq 'CoreServices') {$is_core=1};
# FIXME: If there are several classes, this will generate a Services.h for each, so it will overwrite!

			if (
			($changed eq '1' or (not -e "$cxx_gen_source_path/$class.yml" and $class ne 'CoreServices') ) or
				 ( $class eq 'CoreServices' and $nclasses==1 ) ) {
				say "gprm: generating library configuration $class.yml and wrappers" if $verbose;
				my @task_methods=WrapperGenerator::generate($class,$nclasses,$is_core, $task_name, $task_path, $opts);
				for my $task_method(@task_methods) {
				    system("gpcc-$platform src/GPRM/Task/$task_method.cc -n $nthreads");
				    system("gannetc-$platform -Y $ymlfile  src/GPRM/Task/$task_method.td");
				}
            } else {
                say "gprm: did NOT generate library configuration $class.yml and wrappers!!!" if $verbose;
            }
		}
		my $c=($clean)?'-c':'';
		chdir "$gannet_dir/GPRM/build";
		say "gprm:  generating SystemConfiguration.h from YML files" if $verbose;
# 1. Generate SystemConfiguration from YAML file 
#TODO: this should go into the GannetBuilder
		my $create_config_scons_str="scons $c -f SConstruct.SystemConfiguration.py Y='$ymlpath' D='$cxx_gen_source_path' WD='$wd' $scons_wordsz gen";
        $create_config_scons_str=~s/\s+/ /g;
		say "WD:".cwd() if $verbose;
		say "$create_config_scons_str" if $verbose;
		system($create_config_scons_str);
	}

	if ($build) {
# 2. Build GPRM code 
        if ($build_exe) {
			say "gprm: building GPRM binary" if $verbose;
			say "cd $cxx_build_path" if $verbose;
			chdir "$cxx_build_path";
		} else {
			say "gprm: building GPRM library" if $verbose;
			say "cd $gprm_lib_path" if $verbose;
			chdir "$gprm_lib_path";
		}
		say $run_scons_str if $verbose;
		system($run_scons_str);
# 3. Install binary
        if ($build_exe) {
			say "gprm: installing binary" if $verbose;
			say $run_scons_str," install" if $verbose;
			system("$run_scons_str install");
        }
	}
}
# 4. Build main code
chdir $wd;
system('scons -f SConstruct.gprm');
