#!/usr/bin/env perl
#
################################################################################
#
#   Name: CER7.2.1P2-Launch.pl
#   Author: Brian Magill
#   Created: 8/5/2010
#
#   Purpose:
#     Runs the PCF generator, cleans any existing outputs (if automatically
#     restarted), and executes the run script on the remote host.   
#
#   Revision History:
#     Version 2.0  October 24, 2011
#                  Nelson Hillyer, SSAI, thomas.n.hillyer@nasa.gov
#                  Replaced references to HOSTTYPE with CPUTYPE.  Added PCF output
#                  cleaner as post PCF generator step.  Added sgeLogFooter to
#                  the end of the script to list execution statistics.
#
################################################################################
#
use Toolkit::Utils;
use SGE_Utils qw(sgeLogHeader sgeLogFooter matchPlatform getCPUTYPE);
use CER7_2_1P2_Env qw(getEnvHash);
use PCF_Env qw(setUpPcfHash);
use strict;
					  
my $cputype = getCPUTYPE();
my $status = 0;

sgeLogHeader();

$ENV{'PGSINC'} = matchPlatform($ENV{'PGSINC'}, $cputype);
$ENV{'PGSMSG'} = matchPlatform($ENV{'PGSMSG'}, $cputype);

my %Sarb = getEnvHash();

if (! defined $ARGV[0] or $ARGV[0] !~ /([\d]{4})([\d]{2})Z([\d]{3})/) {
	print "\n================================================================================\n";
	print "= Error: script requires date and zone YYYYMMZxxx as argument\n";
	print "================================================================================\n\n";
	
	$status = 203;
}

my $instance = shift @ARGV;

my $pgeName = $Sarb{PGE};
my ($Sampling_OUT, $Production_OUT, $CCode_OUT) = ($Sarb{SS7_2}, $Sarb{PS7_2}, $Sarb{CC7_2});
my $PCF_File = "$pgeName\_PCF_$Sampling_OUT\_$Production_OUT\_$CCode_OUT\.$instance";

if ($status == 0) {
	my $pcf_script = "$pgeName\_pcfgen.pl";
	my $sys_cmd = "./$pcf_script -date $instance";
	$status = system($sys_cmd) >> 8;
	
	if ($status != 0) {
		print "\n**********  ERROR CREATING PCF FILE, EXITING TEST WITH STATUS = 203 **********\n\n";
		$status = 203;
	}
}

if ($status == 0) {
	$status = Toolkit::Utils::Cleanup_PCF_Outputs(pcf => "pcf/$PCF_File");
}

if ($status == 0) {
	my $run_script = "./run_$pgeName\.pl $PCF_File";
	$status = system($run_script) >> 8;
}

sgeLogFooter(code => $status, lookup => 0);
