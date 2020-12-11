package WrapperGenerator;
use 5.012;
use warnings;
use strict;

our $V=0;
our $gen_src_path='gensrc';
our $src_path='src/GPRM/Kernel';
our $task_path='src/GPRM';
our $templ_path=$ENV{GPRM_DIR}.'/GPRM/build';
our $task_name='Task';

#our $libname;
#our $nclasses;
#our $is_core;
#our %classes;


sub generate {
    my ($libname,$nclasses,$is_core, $task_name, $task_path, $opts) =@_;
    my $kernel_classes={};
	if ($libname ne 'CoreServices') {
        $kernel_classes = parse_class_header($libname, $src_path);
	if (not -e "$src_path/$libname.yml") {
		gen_library_config($kernel_classes, $libname,$nclasses,$is_core );
	}
	}
	gen_kernel_wrapper($kernel_classes, $libname,$nclasses,$is_core);
	
	my @task_methods = gen_task_wrapper($task_name, $task_path);
	
	generate_scons([$task_name],$kernel_classes, $opts);
	return @task_methods;
} # END of generate()

sub gen_library_config {
    my ($classes, $libname,$nclasses,$is_core)=@_;
	my @lines=();
	my $header =
"--- # SBA Core Library Configuration
System:
  Version: 3.0
  Library: $libname

  Services: # last value is control bit (1=control, 0=computational) 
";
	push @lines, $header;
	my $class_id=32; # FIXME: if there are several classes, this is not correct! Every class needs a unique ID
	for my $class (keys %{$classes}) {  
		my $line ="    $class: [ $class_id, kernel_$class, 0 ]\n"; # TODO: only supports computational kernels
		push @lines, $line;
	}
	my $scline = "  ServiceClasses:\n";
	push @lines, $scline;

	for my $class (keys %{$classes}) {  

		my $line="    $class: [ ".join(', ',sort keys %{$classes->{$class}} )." ]\n";
		push @lines, $line;
	}
	if (not -d $gen_src_path) {
		system("mkdir -p $gen_src_path");
	}
	if (not -d 'lib') {
		mkdir 'lib';
	}	
	open my $LC, '>', "$gen_src_path/$libname.yml" unless -e "$src_path/$libname.yml";
	for my $line (@lines) {
		print $LC $line;
	}
	close $LC;
} # END of gen_library_config()

sub gen_kernel_wrapper {
    my ($classes, $libname,$nclasses,$is_core)=@_;
    say "GENERATING $gen_src_path/Services.h" if $V;
	open my $ST,'<',"$templ_path/Services.h"; 
	open my $SG,'>',"$gen_src_path/Services.h";
	while (my $line=<$ST>) {
		if ($line!~/__KERNEL_WRAPPER__/) {
			print $SG $line;
		} else {
			next if $line=~/define/;
			for my $class (keys %{$classes}) {  
				my $header_line = "            void kernel_$class();\n";
				print $SG $header_line;
			}
		}
	}
	close $SG;
	close $ST;
    if (scalar keys %{$classes} == 0) {
	my @lines=();	
	my $ctor_args=''; # FIXME
#		my $libname=$class; # FIXME: in principle they could be different
	my $sig_preamble_line =
"#include \"Services.h\"
#include \"SystemConfiguration.h\"

using namespace SBA;

";
    say "GENERATING $gen_src_path/Services.cc" if $V;

	open my $KW, '>', "$gen_src_path/Services.cc";
	print $KW $sig_preamble_line;
	close $KW;
    
    } else {
    for my $class (keys %{$classes}) {  
    say "CLASS $class $nclasses" if $V;
        my $comment='';
        if ($nclasses>1) {
           if( $libname eq 'CoreServices') {
            next;
           }
        } else {
            if( $libname eq 'CoreServices') {
            $comment='//';
            }
        }

	my @lines=();	
	my $ctor_args=''; # FIXME
#		my $libname=$class; # FIXME: in principle they could be different
 
	my $sig_preamble_line =
"#include \"Services.h\"
$comment#include \"$libname.h\" 
#include \"SystemConfiguration.h\"

using namespace SBA;
using namespace GPRM::Kernel;

void Services::kernel_${class}() {
#ifdef VERBOSE    
    std::cout << \"CALLING kernel_$class\" << std::endl;
#endif    
    ${class}* inst;
    if (init_state(SC_${libname}_${class})) {
#ifdef VERBOSE    
    std::cout << \"INIT ${class}($ctor_args)\" << std::endl;
#endif        
        inst = new ${class}($ctor_args);
        store_state(SC_${libname}_${class},(void*)inst);
    } else {
#ifdef VERBOSE    
    std::cout << \"LOAD ${class}\" << std::endl;
#endif        
        
        inst=(${class}*)load_state(SC_${libname}_${class});
    }

    void* res;
	Symbol_t res_symbol = NIHIL;
#ifdef KERNEL_HAS_STATE	
#ifdef KERNEL_LOCK
    pthread_mutex_t mutex = get_mutex(SC_${libname}_${class});
    pthread_mutex_lock (&mutex);
#endif	
#endif
    switch ( method() ) {
";

    push @lines, $sig_preamble_line;

    for my $method (sort keys%{$classes->{$class}} ) {
    	my $ret_type = $classes->{$class}->{$method}[0];
    	my @arg_types = @{ $classes->{$class}->{$method}[1] };
    	my @args=();
    	my $i=0;
    	my @args_cout=();
    	for my $arg_type (@arg_types) {
    		push @args,"($arg_type)arg($i)";
    		push @args_cout,"arg($i)";
    		$i++;
    	}
    	my $all_args=join(', ',@args);
    	my $args_cout_str=join(' << "," << ',@args);
    	my $line = 
"        case M_${libname}_${class}_${method}:
		{
#ifdef VERBOSE    
    std::cout << \"CALL $method(\" << $args_cout_str << \" )\" << std::endl;
#endif        
		    
			$ret_type retval = inst->$method($all_args);
			res = (void*)retval;
			res_symbol=mkPointerSymbol(res);
			break;
		};
";
	   push @lines, $line;
    }
	   my $postamble_line=
"
		default:
			std::cout << \"ERROR: NO SUCH METHOD: \" << method() << \"for class $class\\n\";
    };
#ifdef KERNEL_HAS_STATE    
#ifdef KERNEL_LOCK
    pthread_mutex_unlock (&mutex);
#endif    
#endif
    result(res_symbol);
}	
";
    push @lines, $postamble_line;
    say "GENERATING $gen_src_path/Services.cc" if $V;

	open my $KW, '>', "$gen_src_path/Services.cc";
	for my $line (@lines) {
		print $KW $line;
	}
	close $KW;
}
}
} # END of gen_kernel_wrapper()

sub parse_class_header {
    (my $libname, my $src_path)=@_;
	my $classheader = "$src_path/$libname.h";
    open my $CH, '<', $classheader or die "$!: $classheader";
    my %classes=();
    my @namespaces=();
    my $class='';
	my %basic_types=(
			'int'=>1,
			'float'=>1,
			'double'=>1,
			'char'=>1,
			'short'=>1,
			'long'=>1,
			);
    
    my $mode='private';
    while(my $line=<$CH>) {
        chomp $line;
        $line=~s/^\s+//;
        $line=~s/\s+$//;
    
        $line=~/^namespace\s+(\w+)\s*\{/ && do {
			my $ns=$1;
            push @namespaces, $ns unless $ns=~/(:?SBA|Base|GPRM|Kernel)/;
            next;
        };

        $line=~/^class\s+(\w+)/ && do {
            $class=$1;
			say "CLASS: $class" if $V;
            next;
        };

        $line=~/(public|private):/ && do {
            $mode=$1;
			say "MODE: $mode" if $V;
            next;
        };

        $line=~/^([:\w\*\&<>]+)\s+(.*?)(\w+)\s*\([^\)]*\)\s*;/ && do {
			say "METHOD: $line" if $V;
            $line=~s/\(\s*\)/(void)/;
            $line=~s/\)\s*;.*$//;
            $line=~s/\(/ (/g;
            my @chunks = split(/\s+\(/,$line);
            my @type_method = split(/\s+/,$chunks[0]);
            my $method=pop @type_method;
			my $ct=0;
			my @templtypes=();
# FIXME: hack for templates. Works, but only if the string __TEMPLATE_TYPE_(\d+)__ does not appear in the original code!
			while ($chunks[1]=~/(<.+?>)/) {
				my $templtype=$1;
				push @templtypes,$templtype;
				$chunks[1]=~s/$templtype/__TEMPLATE_TYPE_${ct}__/;
				$ct++;
			}
            my @arg_types = split(/\s*,\s*/,$chunks[1]);
            my $type = join(' ',@type_method);
			my $fin_arg_types=[];
			for my $arg_type (@arg_types) {
				my @chunks=split(/\s+/,$arg_type);
				if (scalar @chunks != 1) {
					if (not ( ($chunks[0] eq 'const') && (scalar @chunks==2))) {
						my $maybe_var=$chunks[-1];
						$maybe_var=~s/[\*\&]+//;
						
						if (not exists $basic_types{$maybe_var}) {
							pop @chunks;
						}
					}
				}
				$arg_type=join(' ',@chunks);
				if ($arg_type=~/__TEMPLATE_TYPE_(\d+)__/) {
					my $ct=$1;
					$arg_type=~s/__TEMPLATE_TYPE_${ct}__/$templtypes[$ct]/;
				}
				push @{$fin_arg_types},$arg_type;
			}
            say $mode,': ',$type,' ',$method,' (',join(' , ', @arg_types),")" if $V;                    
			if ($mode eq 'public') {
				$classes{$class}{$method}=[$type,$fin_arg_types];				
           }
        };
    }
    close $CH;
    return \%classes;        
}

=pod
I need to create: Task.cc and Task.yml or we could use the names from the GPC method
For Task.cc we need to get the types from Task.h, and then create the wrapper, and write it to gensrc
For Task.yml we need the kernel name from Task.h, and not much else really.
We also need the number of threads but that is independent of the task.

=cut
sub generate_yml {
    (my $task_name, my $task_path, my $nthreads)=@_;
    say "GENERATING YML FILE" if $V;
	open my $TH,'<',"$task_path/$task_name.h" or die "$!:$task_path/$task_name.h"; 
	
	my $klibs=[];
	my $insts={};
	while (my $line=<$TH>) {
		if ($line=~/GPRM::Kernel::(\w+)\s+(\w+)/) {
			push @{$klibs},$1;
			$insts->{$2}=$1;
		} 
	}
	close $TH;
	open my $YML_TEMPL,'<',"$templ_path/GPRM_Task_templ.yml";
	open my $YML,'>',"$task_path/$task_name.yml";
	while (my $line=<$YML_TEMPL>) {
	    if ($line=~/_KLIBS_/ ) {
	        my $klibs_str=join(', ', @{$klibs});
	        $line=~s/_KLIBS_/$klibs_str/;
	    } elsif  ($line=~/_NTHREADS_/) {
			$line=~s/_NTHREADS_/$nthreads/;
	    } elsif  ($line=~/_INST_/) {
	        
	        for my $inst (keys %{$insts}) {
	            my $cline=$line;
	            my $class=$insts->{$inst};
	            $cline=~s/_INST_/$inst/;
	            $cline=~s/_CLASS_/$class/g;
	            print $YML $cline;
	        }
	        next;
	    } elsif  ($line=~/^\s+tn:/) {	        
	        my @services=();
	        for my $inst (keys %{$insts}) {
	        
	            my $class=$insts->{$inst};
	            push @services, "$class.$class";
	        }
	        my $services_str=join(', ',@services);
	        my $cline=$line;	            
	        $cline=~s/_CLASS_\._CLASS_/$services_str/;
	        print $YML $cline;
	        
	        next;
	    }
	    print $YML $line;
	}
	close $YML_TEMPL;
	close $YML;    
} # END of generate_yml()


sub gen_task_wrapper {
    say "GENERATING TASK WRAPPER" if $V;
    (my $task_name, my $task_path)=@_; # Lazy: in principle ppl could choose this name.
    
    my $task_signatures = parse_class_header($task_name,$task_path);

    open my $TASK,'>',"$gen_src_path/$task_name.cc";
    my @task_methods = keys %{ $task_signatures->{$task_name} };
    # Lazy!
    for my $task_method ( @task_methods ) {
        my $task_definition = gen_task_definition( $task_method, $task_name, $task_signatures->{$task_name}->{$task_method} );
        my $tdc_path="$task_path/$task_method.tdc64";
        my $cast_args_res= cast_args_res(  $task_signatures->{$task_name}->{$task_method} );
        (my $cast_args, my $cast_res)=@{$cast_args_res};
        my $return_res='return res;'; 
        if ($cast_res=~/^void\s+res/) {
            $cast_res='';
            $return_res='';
        }
	   open my $TASK_TEMPL,'<',"$templ_path/GPRM_Task_templ.cc";	
	   while (my $line=<$TASK_TEMPL>) {
		if ($line=~/__TASK_NAME__/) {
		  $line=~s/__TASK_NAME__/$task_name/;
		} elsif ($line=~/__TASK_DEFINITION__/) {
		  $line=~s/__TASK_DEFINITION__/$task_definition/;
		} elsif ($line=~/__TDC_PATH__/) {		    
		    $line=~s/__TDC_PATH__/$tdc_path/;
		} elsif ($line=~/__CAST_ARGS__/) {
		    $line=~s/__CAST_ARGS__/$cast_args/;
		} elsif ($line=~/__CAST_RES__/) {
		    $line=~s/__CAST_RES__/$cast_res/;
		} elsif ($line=~/__RETURN_RES__/) {
		    $line=~s/__RETURN_RES__/$return_res/;
		}

        print $TASK $line;
	}
	close $TASK_TEMPL;	    
    }
    close $TASK;
    return @task_methods;
} # END of gen_task_wrapper()

sub gen_task_definition { (my $fname, my $tname, my $sig)=@_;
    (my $rettype, my $argtypes)=@{$sig};
    
    my @args=();
    my $argn=1;
    for my $t (@{$argtypes}) {
        push @args, "$t arg$argn";
        $argn++;
    }
    my $arg_str= join(', ',@args);
    return "$rettype GPRM::${tname}::$fname($arg_str)";    
}

sub cast_args_res { (my $sig)=@_;
    (my $rettype, my $argtypes)=@{$sig};    
    my @args=();
    my $argn=1;
    for my $t (@{$argtypes}) {
        push @args, "void* p${argn}_vp = (void*)arg$argn; args.push_back( p${argn}_vp );";
        $argn++;        
    }
    my $arg_str= 
    my $cast_args=join("\n",@args);
    my $cast_res="${rettype} res = (${rettype})res_vp;";
    return [$cast_args,$cast_res];    
}

sub generate_scons {
(my $task_methods, my $kernel_classes, my $opts) = @_;    
    my $kernel_has_state= exists $opts->{'stateful'}? "'-DKERNEL_HAS_STATE'," : '';
    my $kernel_lock= exists $opts->{'locking'}? "'-DKERNEL_LOCK'," : '';
    my $use_threads = exists $opts->{'nothreads'} ? '' : "'-DUSE_THREADS=1',";
    if (not -e 'GprmConfig.py') {
        my @gprm_srcs=();
            for my $task_name  (@{$task_methods}) {
        push  @gprm_srcs, "'gensrc/$task_name.cc'";
    }
    my $gprm_sources=join(',',@gprm_srcs);
        
    open my $SCONS, '>', 'GprmConfig.py';
    print $SCONS '        
import os
from SCons.Environment import Environment

def gprmConfigEnv():
    GPRM_DIR=os.environ["GPRM_DIR"]

    gprmSources=['.$gprm_sources.']
    cxx=os.environ["CXX"]
    libs=["gannet"]
    LIBpaths=["./lib"]
    INCpaths=["./src","./src/GPRM","./gensrc",GPRM_DIR+"/GPRM/src/SBA",GPRM_DIR+"/GPRM/src",]

    gprmEnv = Environment(CXX = cxx, CXXFLAGS = ['."$use_threads $kernel_has_state $kernel_lock ".'"-DGPRM_API","-O2","-std=c++11","-Wall"],LINKFLAGS=["-pthread"],LIBS=libs,LIBPATH=LIBpaths,CPPPATH=INCpaths, GPRMSRC=gprmSources)
    return gprmEnv
';
close $SCONS;            
    }
    if (not -e 'SConstruct.gprm') {
        open my $SCONS, '>', 'SConstruct.gprm';
        print $SCONS '
from GprmConfig import gprmConfigEnv

env = gprmConfigEnv()

sources=Split("""
src/main.cc
';
        for my $kernel (keys %{$kernel_classes}) {
            say $SCONS "src/GPRM/Kernel/$kernel.cc";
        }
        print $SCONS '""")
sources+=env["GPRMSRC"]
prog=env.Program("main",sources)        
';        
        close $SCONS;    
    }
}


1;
