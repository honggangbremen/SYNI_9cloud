#!/usr/bin/env perl
#$ -S /usr/bin/perl
#
#######################
#
#
# Name:
#   rm_script_7.2.1P2
#
# Description:
#
#  Purpose - To clear all ss7.2.1P2 ouput files in the erbelike
#            directory prior to the execution of ss7.2.1P2 
#
# Input Parameters:
#   argv[1] - The PCF file need for the run.
#
#
# Revision History:
#  Version 2.0   August 27, 1998
#                Ed Kizer, SAIC, e.a.kizer@larc.nasa.gov
#                Initial version of code
#
#######################
#
# *** Determine the Month and Year of data to process
#

use strict;

#      
my $run = $ARGV[0];

my ($year,$month,$zone);
if ($run =~ /(\d{4})(\d{2})Z(\d{3})/) {
     		$year  = $1;
     		$month = $2;
     		$zone  = $3;
}


my @SYNI_Lite_Flags = ('TTT', 'FFF', 'TFF', 'FTF', 'FFT', 'FTT', 'TFT', 'TTF');

my $instance = "$ENV{'SS7_2'}_$ENV{'PS7_2'}_$ENV{'CC7_2'}\.$run";


foreach my $LiteFlag (@SYNI_Lite_Flags) {


$instance = "$ENV{'SS7_2'}_$ENV{'PS7_2'}\-$LiteFlag\_$ENV{'CC7_2'}\.$run";

my $rm_cmd = "rm $ENV{'CERESHOME'}/sarb/CER7.2.1P2/rcf/pcf/CER7.2.1P2_PCF_$instance";
system $rm_cmd;

$rm_cmd = "rm $ENV{'CERESHOME'}/sarb/CER7.2.1P2/rcf/pcf/CER7.2.1P2_PCF_$instance\.log";
system $rm_cmd;

$rm_cmd = "rm $ENV{'CERESHOME'}/sarb/sge_logs/CER7.2.1P2/CER7.2.1P2_$instance\*";
system $rm_cmd;

$rm_cmd = "rm $ENV{'CERESHOME'}/sarb/data/CERES/SYNI/$ENV{'SS7_2'}_$ENV{'PS7_2'}\-$LiteFlag/$year/$month/CER_SYNI_$instance\*";
my @status = system "$rm_cmd > /dev/null";

$rm_cmd = "rm $ENV{'CERESHOME'}/sarb/data/CERES/KQCR/$ENV{'SS7_2'}_$ENV{'PS7_2'}\-$LiteFlag/$year/$month/CER_KQCR_$instance";
@status = system "$rm_cmd > /dev/null";
$rm_cmd = "rm $ENV{'CERESHOME'}/sarb/data/CERES/KQCR/$ENV{'SS7_2'}_$ENV{'PS7_2'}\-$LiteFlag/$year/$month/CER_KQCR_$instance\.met";
@status = system "$rm_cmd > /dev/null";

$rm_cmd = "rm $ENV{'CERESHOME'}/sarb/data/scr/CER_SARBCRH_$instance";
@status = system "$rm_cmd > /dev/null" if -e "$ENV{'CERESHOME'}/sarb/data/scr/CER_SARBCRH_$instance";

# Remove log files

system "rm $ENV{'CERESHOME'}/sarb/runlogs/CER7.2.1P2/CER7.2.1P2_LogStatus_$instance";
system "rm $ENV{'CERESHOME'}/sarb/runlogs/CER7.2.1P2/CER7.2.1P2_LogReport_$instance";
system "rm $ENV{'CERESHOME'}/sarb/runlogs/CER7.2.1P2/CER7.2.1P2_LogUser_$instance";

# Remove comparison output file

system "rm $ENV{'CERESHOME'}/sarb/CER7.2.1P2/test\_suites/CER_SYNI_$instance\.compare";

}

0;
