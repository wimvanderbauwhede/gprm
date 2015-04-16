#!/usr/bin/perl
use 5.012;
use warnings;
use strict;
use Data::Dumper;
#
# :title: Gannet Service-based SoC project - System Configuration module
#
# (c) 2004-2013 Wim Vanderbauwhede <@glasgow.ac.uk>
#
#==============================================================================
#
# Service-based SoC project - C++ System Configuration Generator
#
#==============================================================================
#

# FIXME: these constants must be identical to those in SBA/ServiceConfiguration.h

our $FS_SCLId = 16;
our $FS_SCId  = 8;

use YAML::Syck;

my $help = '
    -v: Verbose
    -Y [YAML SystemConfiguration to be used as input]
    -D [relative path to directory for generated files]
    -C [current working directory]
';

use Getopt::Std;
my %opts;
getopts( 'vhY:D:C:N', \%opts );

our $VERBOSE = $opts{"v"} ? 1 : 0;
our $SBA_YML = '';
if ( $opts{"Y"} ) { $SBA_YML = $opts{'Y'}; }
else              { die "Must specify YML with -Y" }
our $dirpath = "./";
if ( $opts{"D"} ) { $dirpath = $opts{'D'}; }
our $sba_dir = "./";
if ( $opts{"C"} ) { $sba_dir = $opts{'C'}; }

if ( $opts{"h"} ) {
    die $help;
}

our $SBA_WD = $sba_dir;

sub loadLibraryConfig {
    ( my $lib ) = @_;
    say "WD:${SBA_WD}" if $VERBOSE;
    my $libcfg;
    if ( -e "$SBA_WD/gensrc/$lib.yml" ) {
        $libcfg = YAML::Syck::LoadFile("$SBA_WD/gensrc/$lib.yml");
    } elsif ( -e "${SBA_WD}/src/GPRM/Kernel/${lib}.yml" ) {
        $libcfg = YAML::Syck::LoadFile("$SBA_WD/src/GPRM/Kernel/$lib.yml");
    } elsif ( -e $ENV{'GPRM_DIR'} . "/GPRM/src/SBA/Base/$lib.yml" ) {
        $libcfg = YAML::Syck::LoadFile(
            $ENV{'GPRM_DIR'} . "/GPRM/src/SBA/Base/$lib.yml" );
    } else {
        die "Can't find Library Config File ${lib}.yml";
    }
    return $libcfg;
}

=pod
  We need to read the Application config file from . (and only from .)
  Then we lookup up the Library config files in ./Gannet and $GPRM_DIR/SystemConfigurations
  Application config has ServiceNodes and Aliases
  Library config has Services and ServiceClasses
=cut

say "GENERATING SystemConfiguration" if $VERBOSE;
my $appcfg        = YAML::Syck::LoadFile($SBA_YML);
my $libs          = $appcfg->{'System'}{'Libraries'};
my $n_cfg         = $appcfg->{'System'}{'NServiceNodes'};
my $n_actual      = scalar keys %{ $appcfg->{'System'}{'ServiceNodes'} };
my $NServiceNodes = ( $n_actual < $n_cfg ) ? $n_cfg : $n_actual;

#my $i       = 0;
#my $sclibs  = [];
my $libcfgs = [];
for my $lib ( @{$libs} ) {
    push @{$libcfgs}, loadLibraryConfig($lib);

#    if ( -e "$SBA_WD/src/GPRM/Kernel/$lib.h" ) {
#        $sclibs->[$i] =
#          '#include "' . $SBA_WD . '/src/GPRM/Kernel/' . $lib . '.h"';
#    } elsif ( -e $ENV{'GPRM_DIR'} . "/GPRM/src/SBA/Base/$lib.h" ) {
#        $sclibs->[$i] =
#            '#include "'
#          . $ENV{'GPRM_DIR'}
#          . '/GPRM/src/SBA/Base/'
#          . $lib . '.h';
#    } else {
#        say "WARNING: Can't find Library File ${lib}.h";
#    }
#    $i += 1;
}

sub cxx_serviceclasses {
    ( my $appcfg, my $libcfgs ) = @_;    # OK
    my @cxx_service_tuples = ();
    my $prev_sclid_scid    = 0;
    for my $k ( keys %{ $appcfg->{'System'}{'ServiceNodes'} } ) {
        my $entry   = $appcfg->{'System'}{'ServiceNodes'}{$k};
        my $node_id = $entry->[0];
        for my $scname ( @{ $entry->[1] } ) {
            my @serviceclass = split( /\./, $scname );
            my $sclid = 1;
            for my $libcfg ( @{$libcfgs} ) {
                my $libn = $libcfg->{'System'}{'Library'};
                if ( $libn eq $serviceclass[0] ) {
                    my $services = $libcfg->{'System'}{'Services'};
                    for my $sc_str ( keys %{$services} ) {
                        if ( $sc_str eq $serviceclass[1] ) {
                            my $entry            = $services->{$sc_str};
                            my $scid             = $entry->[0];
                            my $core_method_name = $entry->[1];
                            my $sclid_scid       = ( $sclid << 8 ) + $scid;
                            if ( $sclid_scid != $prev_sclid_scid ) {
                                push @cxx_service_tuples,
"\t\tcase ${sclid_scid}:\n\t\t\t${core_method_name}();\n\t\t\tbreak; // $sclid:$scid";
                            }
                            $prev_sclid_scid = $sclid_scid;
                            last;
                        }
                    }
                    last;
                }
                $sclid += 1;
            }
        }
    }
    return join( "\n", @cxx_service_tuples );
}

sub cxx_serviceclass_constants {
    ( my $libcfgs ) = @_;    # OK
    my @serviceclass_constants = ();
    my $sclid                  = 1;
    for my $libcfg ( @{$libcfgs} ) {
        my $libn     = $libcfg->{'System'}{'Library'};
        my $services = $libcfg->{'System'}{'Services'};
        for my $sc_str ( keys %{$services} ) {
            my $entry = $services->{$sc_str};

# FIXME: rather ad-hoc, if (it's a CTRL service there can be only one, if (it's a COMP service the ServiceManager doesn't need to know
            my $ctrl_or_comp = $entry->[2];
            my $scid         = $entry->[0];
            my $sclid_scid   = ( $sclid << 8 ) + $scid;
            my $sc_lib       = $libcfg->{'System'}{'Library'};
            push @serviceclass_constants,
              "const UINT SC_${sc_lib}_${sc_str} = ${sclid_scid}; // $sclid:$scid";
        }
        $sclid += 1;
    }
    return join( "\n", @serviceclass_constants );
}

# FIXME: This is a hack for temporary compatibility. It assigns the node id to the first service class in the list
# c3: [ 3, [LET] ]
# This will break for multi-service nodes: c3: [3, Service1, LET, IF, ALU]
sub servicenodes {
    ( my $appcfg ) = @_;    # OK
    my $tServiceNodes = {};
    for my $k ( keys %{ $appcfg->{'System'}{'ServiceNodes'} } ) {
        my $entry         = $appcfg->{'System'}{'ServiceNodes'}{$k};
        my $node_id       = $entry->[0]; # e.g. 1
        my $service_classes = $entry->[1]; # e.g. [CoreServices.SEQ, ...]
        $tServiceNodes->{$k}         = {};                
        $tServiceNodes->{$k}{'Names'} = $service_classes;
        $tServiceNodes->{$k}{'Addr'}  = $node_id;
    }
    return $tServiceNodes;
}

# FIXME: This is a hack for temporary compatibility. It assigns the node id to the first service class in the list
# c3: [ 3, [LET] ]
# This will break for multi-service nodes: c3: [3, Service1, LET, IF, ALU]
sub cxx_service_addresses {
    ( my $appcfg ) = @_;    # OK
    my @tServiceNodes = ();
    for my $k ( keys %{ $appcfg->{'System'}{'ServiceNodes'} } ) {
        my $entry       = $appcfg->{'System'}{'ServiceNodes'}{$k};
        my $node_id = $entry->[0];
        push @tServiceNodes, $node_id;
    }
    if ( scalar @tServiceNodes < $NServiceNodes ) {
        for my $k ( scalar @tServiceNodes .. $NServiceNodes - 1 ) {
            push @tServiceNodes, "1";    # FIXME very ad-hoc
        }
    }
    return join( ",", @tServiceNodes );
}

sub cxx_method_constants {
    ( my $libcfgs ) = @_;                # OK
    my @method_constants = ();
    my $sclid            = 1;
    for my $cfg ( @{$libcfgs} ) {
        my $sc_lib = $cfg->{'System'}{'Library'};
        for my $serviceclass ( keys %{ $cfg->{'System'}{'ServiceClasses'} } ) {
            my $scid        = $cfg->{'System'}{'Services'}{$serviceclass}[0];
            my $scid_field  = $scid << $FS_SCId;
            my $sclid_field = $sclid << $FS_SCLId;
            my $opcode =
              1;    # The compiler starts opcodes at 1, so 0 is an error code
            for
              my $methname ( @{ $cfg->{'System'}{'ServiceClasses'}{$serviceclass} } )
            {
                push @method_constants,
                  "const UINT M_${sc_lib}_${serviceclass}_${methname} = "
                  . ( $sclid_field + $scid_field + $opcode ) . "; // $sclid:$scid:$opcode";
                $opcode += 1;
            }
        }
        $sclid += 1;
    }
    return join( "\n", @method_constants );
}

sub cxx_servicenode_constants {
    ( my $appcfg, my $libcfgs ) = @_;    # OK

    my @servicenode_constants = ();
    my $sns                   = servicenodes($appcfg);    
    for my $node_id ( keys %{$sns} ) {
        my $servicenode_name_strs = $sns->{$node_id}{'Names'};        
        my $addr = $sns->{$node_id}{'Addr'};
        for my $servicenode_name_str (@{$servicenode_name_strs}) {
        if ( $servicenode_name_str =~ /\.LET$/ ) {
            push @servicenode_constants, "const UINT S_LET = $addr;";
        }
        if ( $servicenode_name_str =~ /\.IF$/ ) {
            push @servicenode_constants, "const UINT S_if (= $addr;";
        }
        my $const_name_str = $servicenode_name_str;
        $const_name_str =~ s/\./_/g;
        my @serviceclass = split( /\./, $servicenode_name_str );
        my $sc_lib       = $serviceclass[0];
        my $sc_str       = $serviceclass[1];
        for my $cfg ( @{$libcfgs} ) {
            if ( $sc_lib eq $cfg->{'System'}{'Library'} ) {
                my $services = $cfg->{'System'}{'Services'};
                my $entry    = $services->{$sc_str};

# FIXME: rather ad-hoc, if (it's a CTRL service there can be only one, if (it's a COMP service the ServiceManager doesn't need to know
                my $ctrl_or_comp = $entry->[2];
                if ( $ctrl_or_comp == 1 ) {
                    push @servicenode_constants,
                      "const UINT S_${const_name_str} = $addr; ";
                }
            }
        }
        }
    }
    return join( "\n", @servicenode_constants );

}    # of cxx_servicenode_constants()

# ====================================================================

my $cxxh_file = "$dirpath/SystemConfiguration.h";

open my $cxxh, '>', $cxxh_file;

say $cxxh '
/** \file SystemConfiguration.h
   
 \brief Gannet Service-based SoC project - C++/SystemC System Configuration
        
        Generated from SBA.yml with create_Cxx_SystemConfiguration.rb

  (c) 2008-2012 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>
    
*/

//==============================================================================
//
// System Configuration
//
// GENERATED from YAML configuration using create_Cxx_SystemConfiguration.rb
//
//==============================================================================

// 

#ifndef _SBA_SYSTEM_CONFIGURATION_H_
#define _SBA_SYSTEM_CONFIGURATION_H_

using namespace std;

typedef unsigned int UINT;

namespace SBA {

';
say $cxxh cxx_method_constants($libcfgs);
say $cxxh cxx_servicenode_constants( $appcfg, $libcfgs );
say $cxxh cxx_serviceclass_constants($libcfgs);
say $cxxh
  '// Not elegant, but static arrays are a lot faster than linked lists!';
say $cxxh "const UINT NSERVICES = ${NServiceNodes};";

#say $cxxh "const UINT SERVICE_ADDRESSES[${NServiceNodes}]={${cxx_service_addresses(appcfg)}};"

say $cxxh "     
} // SBA
#endif /*_SBA_SYSTEM_CONFIGURATION_H_*/
";

close $cxxh;

my $cxx_file = "$dirpath/SelectWrapper.cc";

open my $cxx, '>', $cxx_file or die "$!: $cxx_file";

say $cxx '
#include "Services.h"

void Services::select_wrapper(unsigned int code) {

	switch (code) {
';
say $cxx cxx_serviceclasses( $appcfg, $libcfgs );
say $cxx '		default:
			none();
	};
}
';

close $cxx;

