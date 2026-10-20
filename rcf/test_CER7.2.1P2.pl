#!/usr/bin/env perl
#
#######################
#
#
# Name:
#   test_7.2.1.pl
#
# Description:
#
#  Purpose - To run the Synoptic SARB Subsystem test cases for various flag conditions
#
# Revision History:
#
# Version 1.0    June 22, 2020
#                Tom Caldwell, SSAI, Thomas.E.Caldwell@nasa.gov
#                Initial version of code
#
#########################################################################
#

use strict;
use warnings;


my $ZONE = "1";
my $CZONE = "Z001";
my $PCFINSTANCE = "$ENV{DATE}$CZONE";

my @Entropy_Flags  = ('true', 'false', 'true',  'false', 'false', 'false', 'true',  'true');
my @Pristine_Flags = ('true', 'false', 'false', 'true',  'false', 'true',  'false', 'true');
my @Tuning_Flags   = ('true', 'false', 'false', 'false', 'true',  'true',  'true',  'false');

my @SYNI_Lite_Flags = ('TTT', 'FFF', 'TFF', 'FTF', 'FFT', 'FTT', 'TFT', 'TTF');

my $TestPS = "$ENV{'PS7_2'}";

my @a = (0,1,2,3,4,5,6,7);
foreach my $index (@a){

$ENV{'EntropyCode'} = $Entropy_Flags[$index];
$ENV{'PristineCode'} = $Pristine_Flags[$index];
$ENV{'TuningCode'} = $Tuning_Flags[$index];
$ENV{'PS7_2'} = $TestPS.'-'.$SYNI_Lite_Flags[$index];

run_test_case($PCFINSTANCE);

}

#exit ($PGE_EXIT); 
exit; 


sub run_test_case{
    my ($INSTANCE) = @_;

my $PCF_filename = "CER7.2.1P2_PCF\_$ENV{'SS7_2'}\_$ENV{'PS7_2'}\_$ENV{'CC7_2'}\.$INSTANCE";
my $exit_1 = system "./cleanup_7.2.1P2.pl $PCF_filename";

my $PCFEXE = "$ENV{'CERESHOME'}/sarb/CER7.2.1P2/rcf/CER7.2.1P2_pcfgen.pl -date $INSTANCE";
$exit_1 = system $PCFEXE;

if ( $exit_1 == 0) {

#        my $SYNEXE = "./run_CER7.2.1P2.pl $PCF_filename";
        my $SYNEXE = "./sge_sync.pl $ENV{'CERESHOME'}/sarb/CER7.2.1P2/rcf/CER7.2.1P2-SGE_Driver.pl -date $ENV{DATE} -zone $ZONE -clean";
        $exit_1 = system $SYNEXE;

        if ( $exit_1 == 0) {
         print "Test Case $ENV{'PS7_2'} was successful\n";}	
        else {
         print "Problem with Test Case $ENV{'PS7_2'}\n";	
         exit($exit_1);
	}
}
else {
        print "Problem with PCF $ENV{'PS7_2'}\n";	
        exit($exit_1);
}
}



