#!/usr/bin/env perl
#
#######################
#
#
# Name:
#   pcfgen_7.2.1P2
#
# Description:
#
#  Purpose - This script generates a PCF file for SARB PGE 7.2.1P2.
#
# Input Parameters:
#   argv[1] - A monthly date in the form yyyyMMZZZZ
#               where yyyy = 4 digit year
#                       MM = 2 digit month
#                     ZZZZ = 3 digit zone ex: Z180
#
# Revision History:
#  Version 2.0    October 26, 2011
#                 Nelson Hillyer, SSAI, thomas.n.hillyer@nasa.gov
#                 Migrated PCF routines to PCF module.
#  Version 1.2    T. Nelson Hillyer, SSAI (thomas.n.hillyer@nasa.gov)
#                 September 28, 2011
#                 Changed the ECS, ESNOW, and EICE input data products to be
#                 flagged as a "Mandatory" input.
#  Version 1.1    T. Nelson Hillyer, SSAI (thomas.n.hillyer@nasa.gov)
#                 August 2, 2011
#                 Fixed error in the way that Get_Days_In_Month is being called
#                 which would cause the wrong number of days to be given for a
#                 month in certain cases.
#  Version Z      June, 18, 2009
#                 Scott Zentz, SSAI
#######################

use strict;
use Getopt::Long;
use Dates;
use Toolkit::PCF;
use Log::PCF;
use CER7_2_1P2_PCF;
use CER7_2_1P2_Env;
use CER7_2_1P2_FileUtils;

my %Sarb = CER7_2_1P2_Env::getEnvHash();
my ($date);

GetOptions('date:s' => \$date);

# Create a new, empty PCF object.
my $pcf = Toolkit::PCF->new();

# Populate the PCF object.
CER7_2_1P2_PCF::ConstructPCF(pcf => $pcf, environment => \%Sarb, instance => $date);

# Write the PCF object to disk
$pcf->writePCF(file => "$Sarb{'pcfdir'}/$Sarb{'pcfname_prefix'}.$date");

# Create a PCF log object.
my $pcflog = Log::PCF->new(pcf => $pcf);

# Write the PCF log object to disk.
$pcflog->writePCFLog(file => "$Sarb{'pcfdir'}/$Sarb{'pcfname_prefix'}.$date.log", environment => \%Sarb);

# Return a 203 exit code if the FileUtils::checkInput subroutine isn't happy.
if (CER7_2_1P2_FileUtils::checkInput(pcf => $pcf, environment => \%Sarb, instance => $date) != 0) {
	print "PCF Failure\n";
	print "Exit code = 203\n";
	print "Please stage missing file(s) if available\n";
	print "Check log file for more information:\n$Sarb{'pcfdir'}/$Sarb{'pcfname_prefix'}.$date.log\n";
	print "Exiting with code 203\n";
	exit(203);
}

# Return 0 since everything was successful.
exit(0);
