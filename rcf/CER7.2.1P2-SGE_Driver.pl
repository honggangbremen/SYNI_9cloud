#!/usr/bin/env perl
#
################################################################################
#
#   Name: CER7.2.1P2-SGE_Driver.pl
#   Author: Brian Magill
#   Created: 8/5/2010
#
#   Purpose:
#   Main processing script for CER7.2.1P2.  
#
#   Accepts a single date and zonal options as arguments.  Loops through 
#   all of the zones specified checking to see if the input files exist.  If they do, 
#   the submit script is called for this date. If necessary files are missing, 
#   no job is submitted, but an error log is generated.    
#
#  Command line arguments:
#   -date YYYYMM                   # Run a single month.
#   -start YYYYMM -end YYYYMM      # Run a range of months.
#   -zone ZZZ                      # Run a single zone.
#   -start_zone ZZZ -end_zone ZZZ  # Run a range of zones.
#   -platform CPU                  # Specify the target execution platform.
#   -clean                         # Clean pre-existing outputs.
#   -help                          # Prints a friendly usage message.
#
#   Note:  All zones must be between 1 and 180. 
# 
#   !Revision History:
#   Version 2.0   October 26, 2011
#                 Nelson Hillyer, SSAI, thomas.n.hillyer@nasa.gov
#                 Swapped SGE_Driver functionality out with new Perl_Lib handlers.
#   Version 1.1     T. Nelson Hillyer, SSAI (thomas.n.hillyer@nasa.gov)
#                   September 28, 2011
#                   Added a call to the PCF::Reset() subroutine to
#                   empty out the PCF log file contents between
#                   separate SGE instance submissions.
#   $Log:$
#
################################################################################
#
use CERES::AJSS_PGE;
use CERES::PCFChecks qw(checkForExistingFiles);
use CER7_2_1P2_PCF qw(ConstructPCF);
use CER7_2_1P2_FileUtils qw(checkInput);
use CER7_2_1P2_Env qw(getEnvHash);
use strict;

my %Sarb = getEnvHash();

my $ajss_pge = CERES::AJSS_PGE->new(type => $Sarb{'pgetype'}, env => \%Sarb,
                                    generatePCF => \&ConstructPCF,
				    checkOutput => \&checkForExistingFiles,
				    checkInput  => \&checkInput,
				    resources => {m_mem_free => '500M', virtual_free => '700M'});
									
$ajss_pge->processInstances();
