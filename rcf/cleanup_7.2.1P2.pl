#!/usr/bin/env perl
#
###########################################
#
#  NAME: 
#    rm_script_7.2.1P2.pl 
#
#  Purpose - script which removes output files produced by
#            PGE CER7.2.1P2 during testing.  Failure to
#            remove these files between test runs of
#            the same hour results in failure of the
#            subsystem, as the logic tries to open the
#            output files as new files.
#
# Input Parameters:
#   ARGV[0] - PCF filename
#
#  USAGE: 
#    rm_script_7.2.1P2.pl PCF_name
#
# Revision History:
#
#  Version 1.0   November 18, 2005
#                Tom Caldwell, SAIC, t.e.caldwell@larc.nasa.gov
#                Jim Donaldson, NASA, j.l.donaldson@larc.nasa.gov
#                Scott Zentz, SAIC, s.m.zentz@larc.nasa.gov
#                Initial version of code
#
###########################################
#
#
   $_ = $ARGV[0];
   s/CER7\.2\.1P2_PCF_//;
   my $SYN_INSTANCE = $_;

   my $CERPGENam = 'CER7.2.1P2';

   my ($yyyy,$mm,$zzz);
   if ($SYN_INSTANCE =~ /(\d{4})(\d{2})Z(\d{3})/) {
		   $yyyy = $1;
		   $mm   = $2;
		   $zzz   = $3;
   }

# Remove pcf-related files

   my $PCFgenDir = "$ENV{'CERESHOME'}/sarb/CER7.2.1P2/rcf/pcf";
   my $PCFDir = "$ENV{'CERESHOME'}/sarb/CER7.2.1P2/rcf/pcf";
   system "rm $PCFDir/$CERPGENam\_PCF_$SYN_INSTANCE";
   system "rm $PCFDir/$CERPGENam\_PCF_$SYN_INSTANCE.log";

# Remove subsystem data files

   my $DataDir = "$ENV{'CERESHOME'}/sarb/data/CERES/SYNI/$ENV{'SS7_2'}_$ENV{'PS7_2'}/$yyyy/$mm";
   system "rm $DataDir/CER_SYNI_$SYN_INSTANCE";
   system "rm $DataDir/CER_SYNI_$SYN_INSTANCE.met";

# Remove QC files

   my $QADir = "$ENV{'CERESHOME'}/sarb/data/CERES/KQCR/$ENV{'SS7_2'}_$ENV{'PS7_2'}/$yyyy/$mm";
   system "rm $QADir/CER_KQCR_$SYN_INSTANCE";
   system "rm $QADir/CER_KQCR_$SYN_INSTANCE.met";

# Remove log files

   my $LogsDir = "$ENV{'CERESHOME'}/sarb/runlogs/$CERPGENam";
   system "rm $LogsDir/$CERPGENam\_LogStatus_$SYN_INSTANCE";
   system "rm $LogsDir/$CERPGENam\_LogReport_$SYN_INSTANCE";
   system "rm $LogsDir/$CERPGENam\_LogUser_$SYN_INSTANCE"; 

# Remove input data files
   my $SCRDir = "$ENV{'CERESHOME'}/sarb/data/scr"; 
   system "rm $SCRDir/CER_SARBCRH_$SYN_INSTANCE";   

