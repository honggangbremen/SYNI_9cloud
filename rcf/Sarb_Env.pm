package Sarb_Env;
#
################################################################################
#
#   Name: Sarb_Env.pm
#   Author: Brian Magill
#   Created: 8/5/2010
#
#   Purpose:
#   Sets up SARB specific directories
#
#   !Revision History:
#   $Log:$
#
################################################################################
#

require Exporter;

our @ISA = qw(Exporter);
our @EXPORT_OK = qw(getSubSysHash);
our $VERSION = 1.00;

use strict;
use warnings;
use Cwd;
use PathUtils qw(parentDir);


sub getSubSysHash {

  my $currDir =  cwd();
  my $home = parentDir(cwd(), 2);  
  
  my $InstrArchDir = "$home/data";
  my $InputArchive = defined $ENV{InputArchive} ? $ENV{InputArchive} : "/ASDC_archive/CERES";
#  my $localArchive = defined $ENV{LocalArchive} ? $ENV{LocalArchive} : "local_archive";  #### NEED TO FIX THIS
  
	my %sarb =  ('SARB_HOME' => "$home",
	            'Sarblib'      => "$home/lib",
	            'InputArchive'  => "$InputArchive",
	            'MainRCF'       => "$home/rcf",
              'ANCDIR'  => "$home/ancillary",
#              'MCFDIR'        => "$home/rcf/mcf",
              'MCFDIR'        => "$currDir/mcf",
              'LOGDIR'        => "$home/runlogs",
#              'InstrCompare'  => "$home/test_suites",
#              'InstrInterDir' => "$home/data",
#              'InstrArchDir'  => "$home/data",
#              'InstrWebDir'   => "$home/web",
	            'DS_FILE_PATH'  => "$home/data/input",	  
              'SGELOGdir'     => "$home/sge_logs",
              'PCFDIR'        => "$currDir/pcf",
              'SCRDIR'        => "$home/data/scr",
              'GGEO_Dir'      => "/ASDC_archive/CERES",
              'RUNDIR'        => ".",
              );

  wantarray ? return %sarb : return \%sarb;  
  }
    
    	
1;
