#!/usr/bin/env perl
#
#######################
#
#
# Name:
#   run_7.2.1.pl
#
# Description:
#
#  Purpose - To start the process to generate the Synoptic SARB Subsystem.
#
# Input Parameters:
#   ARGV[0] - The Process Control File CER7.2P1_PCF_$SS7_2_$PS7_2_$CC7_2.YYYYMMZxxx
#                 where, SS7_2  = Sampling Strategy'
#                        PS7_2  = Production Strategy'
#                        CC7_2  = Configuration Code'
#                        YYYY = 4 digit year'
#                          MM = 2 digit month'
#                         "Z" = Character constant
#                         xxx = 3 digit 1-degree colatitudinal zone index  
#
#
# Revision History:
#
# Version 1.1    October 26, 2011
#                Nelson Hillyer, SSAI, thomas.n.hillyer@nasa.gov
#                Replaced references HOSTTYPE to CPUTYPE.
# Version 1.0    November 18, 2005
#                Tom Caldwell, SAIC, t.e.caldwell@larc.nasa.gov
#                Jim Donaldson, NASA, j.l.donaldson@larc.nasa.gov
#                Scott Zentz, SAIC, s.m.zentz@larc.nasa.gov
#                Initial version of code
#
#########################################################################
#

use strict;
use warnings;

use lib "../../rcf/";
use Sarb_Env qw(getSubSysHash);
use PCF_Env qw(setUpPcfHash);

my $hostlocal = "x86_64";

my $sarbHome = getSubSysHash()->{SARB_HOME};

my %Vault = setUpPcfHash($ENV{'CERESHOME'}, $ENV{'PGSMSG'}, $ENV{'PGSINC'}, $ENV{'CPUTYPE'});
$ENV{LD_LIBRARY_PATH} = $Vault{LD_LIBRARY_PATH};
$ENV{TK_Ver} = $Vault{TK_Ver};
$ENV{PGSDIR} = $Vault{PGSDIR};


if (scalar(@ARGV) != 1) {

	print "\n====================================================================================\n";
	print "= Error script requires pcf filename as input : $0 <PCF filename>       =\n";
	print "====================================================================================\n\n";

	exit (215);
}

my $PCF_filename = $ARGV[0];   

my $chopname = $PCF_filename;
$chopname =~s/CER7.2.1P2_PCF_//;

my $PCFile = "$sarbHome/CER7.2.1P2/rcf/pcf/$PCF_filename";

$ENV{'PGS_PC_INFO_FILE'} = $PCFile;

my $SYNEXE = "$sarbHome/CER7.2.1P2/bin/CER7.2.1P2_$hostlocal.exe";

my $exit_1 = system $SYNEXE;
my $PGE_EXIT;

my $crhFile = "$sarbHome/data/scr/CER_SARBCRH_$chopname";
unlink $crhFile if (-e $crhFile);

if ( $exit_1 == 0) {
	$PGE_EXIT = 0;
}
else {
	$PGE_EXIT = 202;
}

print "SYNOPTIC SARB SUBSYSTEM MAIN-PROCESSOR PGE_EXIT = $PGE_EXIT\n";
exit ($PGE_EXIT); 
