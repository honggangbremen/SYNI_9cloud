package CER7_2_1P2_Env;
#
################################################################################
#
#   Name: CER7_2_1P2_env.pm
#   Author: Brian Magill
#   Created: 8/5/2010
#
#   Purpose:
#   Copies the SGE environmental variables specific to CER7_2_1P2_env from the
#   session's environment to the catalog hash.  These variables are mostly for
#   cataloguing purposes.   
#
# Revision History:
#  Version 2.0   October 26, 2011
#                Nelson Hillyer, SSAI, thomas.n.hillyer@nasa.gov
#                Merged the subsystem hashes and added AJSS_ENV standard entries.
#                Added argToEnv subroutine to shove certain command line
#                arguments into the PGE hash.
################################################################################
#
use Exporter;
use lib '../../rcf';
use CERES::AJSS_ENV;
use Sarb_Env;
use Cwd;
use strict;
use warnings;

our @ISA 	= qw(Exporter);
our @EXPORT_OK 	= qw($INPUT_CHECK $OPTION_CODE getEnvHash);						
our $VERSION 	= 1.00;

sub getEnvHash {
    my %Sarb = CERES::AJSS_ENV::buildEnvironment();	
    my %SubSysHash = Sarb_Env::getSubSysHash();

    %Sarb = (%Sarb, %SubSysHash);

    $Sarb{PGE}        = 'CER7.2.1P2';
    $Sarb{SS7_2}      = $ENV{SS7_2};     
    $Sarb{PS7_2}      = $ENV{PS7_2};     
    $Sarb{CC7_2}      = $ENV{CC7_2};     
    $Sarb{SS7_3_4}    = $ENV{SS7_3_4};     
    $Sarb{PS7_3_4}    = $ENV{PS7_3_4};     
    $Sarb{CC7_3_4}    = $ENV{CC7_3_4};     
    $Sarb{SS5}        = $ENV{SS5};         
    $Sarb{PS5}        = $ENV{PS5};         
    $Sarb{CC5}        = $ENV{CC5};         
       
    $Sarb{SS5_2}    = "null";
    $Sarb{PS5_2}    = "null";
    $Sarb{CC5_2}    = "null";

    if (lc $ENV{SS5_2} ne "null" ) {
        $Sarb{SS5_2}    = $ENV{SS5_2};
        $Sarb{PS5_2}    = $ENV{PS5_2};
        $Sarb{CC5_2}    = $ENV{CC5_2};
    }

    $Sarb{SS5_5}        = $ENV{SS5_5};         
    $Sarb{PS5_5}        = $ENV{PS5_5};         
    $Sarb{CC5_5}        = $ENV{CC5_5};         

    $Sarb{SS12}       = $ENV{SS12};       
    $Sarb{PS12}       = $ENV{PS12};       
    $Sarb{CC12}       = $ENV{CC12};       
    $Sarb{SS4_2}      = $ENV{SS4_2};     
    $Sarb{PS4_2}      = $ENV{PS4_2};     
    $Sarb{CC4_2}      = $ENV{CC4_2};     
    $Sarb{SS4_0}      = $ENV{SS4_0};     
    $Sarb{PS4_0_8}    = $ENV{PS4_0_8};     
    $Sarb{CC4_0P1_8}  = $ENV{CC4_0P1_8};     
    
    $Sarb{PS4_0_16} = "null";     
    $Sarb{CC4_0P1_16} = "null";     
    
    if (defined $ENV{PS4_0_16}) {$Sarb{PS4_0_16}   = $ENV{PS4_0_16}};     
    if (defined $ENV{CC4_0P1_16}) {$Sarb{CC4_0P1_16}   = $ENV{CC4_0P1_16}};

    $Sarb{SAT}        = $ENV{SAT};         
    $Sarb{INST}       = $ENV{INST};       
    $Sarb{IMAG}       = $ENV{IMAG};       
    $Sarb{SS_MATCH}   = $ENV{SS_MATCH};
    $Sarb{PS_MATCH}   = $ENV{PS_MATCH};
    $Sarb{CC_MATCH}   = $ENV{CC_MATCH};
    $Sarb{EntropyCode}   = $ENV{EntropyCode};
    $Sarb{PristineCode}   = $ENV{PristineCode};
    $Sarb{TuningCode}   = $ENV{TuningCode};
    $Sarb{MCD_Col}   = $ENV{MCD_Col};
    $Sarb{SW7_2}      = $ENV{SW7_2};     
    $Sarb{DATA7_2}    = $ENV{DATA7_2};   
    $Sarb{InputCheck} = $ENV{InputCheck};
    $Sarb{OutputArchive} = $ENV{OutputArchive};

    my $current_dir = cwd();
    chdir "../..";
    chomp(my $sarb_home = cwd());
    chdir $current_dir;

    $Sarb{'SARB_HOME'} = $sarb_home;

    # Fill in variables need by AJSS_ENV
    $Sarb{pgetype} = "monthly-zonal";
    # $Sarb{InputArchive};  # ALREADY DEFINED IN Sarb_Env!
    $Sarb{InputArchiveSS} = "";
    $Sarb{InputArchiveInt} = "";
#	$Sarb{platform} = "p6, x86";
    $Sarb{platform} = "x86";
    $Sarb{pcfdir} = "$Sarb{'SARB_HOME'}/$Sarb{'PGE'}/rcf/pcf";
    $Sarb{sgelogdir} = "$Sarb{'SARB_HOME'}/sge_logs/$Sarb{'PGE'}";
    $Sarb{runlogdir} = "$Sarb{'SARB_HOME'}/runlogs/$Sarb{'PGE'}";
    $Sarb{bindir} = "$Sarb{'SARB_HOME'}/$Sarb{'PGE'}/bin";
    $Sarb{executables} = "$Sarb{'bindir'}/CER7.2.1P2_x86_64.exe";
#	
    $Sarb{pcfname_prefix} = "$Sarb{'PGE'}\_PCF_$Sarb{'SS7_2'}\_$Sarb{'PS7_2'}\_$Sarb{'CC7_2'}";
    $Sarb{instance_prefix} = "$Sarb{'PGE'}\_$Sarb{'SS7_2'}\_$Sarb{'PS7_2'}\_$Sarb{'CC7_2'}";

    return %Sarb;
}

1;
