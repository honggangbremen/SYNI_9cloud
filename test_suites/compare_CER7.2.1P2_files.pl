#!/usr/bin/perl -w
#$ -S /usr/bin/perl
#
#  Name:
#    compare_CER7.2.1P2_files.pl
#
#

my $date = "200110";
my $Exp_file = "CER_SYNI_$ENV{'SS7_2'}_$ENV{'PS7_2'}_$ENV{'CC7_2'}.$date";
my $Prod_file = "CER_SYNI_$ENV{'SS7_2'}_$ENV{'PS7_2'}_$ENV{'CC7_2'}.$date";       

my @SYNI_Lite_Flags = ('TTT', 'FFF', 'TFF', 'FTF', 'FFT', 'FTT', 'TFT', 'TTF');


foreach my $LiteFlag (@SYNI_Lite_Flags) {

if ($ENV{'SS7_2'} eq "Terra-MODIS"){

$date = "200110Z005"; 
$Exp_file = "CER_SYNI_$ENV{'SS7_2'}_$ENV{'PS7_2'}\-$LiteFlag\_$ENV{'CC7_2'}.$date";
$Prod_file = "CER_SYNI_$ENV{'SS7_2'}_$ENV{'PS7_2'}\-$LiteFlag\_$ENV{'CC7_2'}.$date";       
system "./run_syni_compare.pl $Exp_file $Prod_file $LiteFlag\n";
}


if ($ENV{'SS7_2'} eq "Terra-Aqua-MODIS"){
$date = "202108Z005";
$Exp_file = "CER_SYNI_$ENV{'SS7_2'}_$ENV{'PS7_2'}\-$LiteFlag\_$ENV{'CC7_2'}.$date";
$Prod_file = "CER_SYNI_$ENV{'SS7_2'}_$ENV{'PS7_2'}\-$LiteFlag\_$ENV{'CC7_2'}.$date";       
system "./run_syni_compare.pl $Exp_file $Prod_file $LiteFlag\n";
} 

if ($ENV{'SS7_2'} eq "Aqua-MODIS"){
$date = "201907Z005";
$Exp_file = "CER_SYNI_$ENV{'SS7_2'}_$ENV{'PS7_2'}\-$LiteFlag\_$ENV{'CC7_2'}.$date";
$Prod_file = "CER_SYNI_$ENV{'SS7_2'}_$ENV{'PS7_2'}\-$LiteFlag\_$ENV{'CC7_2'}.$date";       
system "./run_syni_compare.pl $Exp_file $Prod_file $LiteFlag\n";
} 

if ($ENV{'SS7_2'} eq "NPP-FM5-VIIRS"){
$date = "201304Z005";
$Exp_file = "CER_SYNI_$ENV{'SS7_2'}_$ENV{'PS7_2'}\-$LiteFlag\_$ENV{'CC7_2'}.$date";
$Prod_file = "CER_SYNI_$ENV{'SS7_2'}_$ENV{'PS7_2'}\-$LiteFlag\_$ENV{'CC7_2'}.$date";       
system "./run_syni_compare.pl $Exp_file $Prod_file $LiteFlag\n";
} 

if ($ENV{'SS7_2'} eq "Terra-NPP"){
$date = "201304Z005";
$Exp_file = "CER_SYNI_$ENV{'SS7_2'}_$ENV{'PS7_2'}\-$LiteFlag\_$ENV{'CC7_2'}.$date";
$Prod_file = "CER_SYNI_$ENV{'SS7_2'}_$ENV{'PS7_2'}\-$LiteFlag\_$ENV{'CC7_2'}.$date";       
system "./run_syni_compare.pl $Exp_file $Prod_file $LiteFlag\n";
} 

if ($ENV{'SS7_2'} eq "NOAA20-FM6-VIIRS"){
$date = "201907Z005";
$Exp_file = "CER_SYNI_$ENV{'SS7_2'}_$ENV{'PS7_2'}\-$LiteFlag\_$ENV{'CC7_2'}.$date";
$Prod_file = "CER_SYNI_$ENV{'SS7_2'}_$ENV{'PS7_2'}\-$LiteFlag\_$ENV{'CC7_2'}.$date";       
system "./run_syni_compare.pl $Exp_file $Prod_file $LiteFlag\n";
} 

}
