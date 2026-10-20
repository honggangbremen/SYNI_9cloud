#!/usr/bin/perl -w
#$ -S /usr/bin/perl
#
#  Name:
#    run_syni_compare.pl
#
#
# Mandatory input arguments:
#    $0 - SYNI filename from expected output
#    $1 - SYNI filename from test 
#
#
      my $hostlocal = "x86_64";

      my $test_path = "$ENV{'CERESHOME'}/sarb/CER7.2.1P2/test\_suites";

      system "rm $test_path/SYNI_Expected\n"     if (-e "$test_path/SYNI_Expected"); 
      system "rm $test_path/SYNI_Prod\n"         if (-e "$test_path/SYNI_Prod"); 
      system "rm $test_path/Diag_Out\n"          if (-e "$test_path/Diag_Out"); 
      system "rm $test_path/tfile\n"             if (-e "$test_path/tfile"); 
      system "rm $test_path/Input\n"             if (-e "$test_path/Input"); 
      system "rm $test_path/filenames\n"         if (-e "$test_path/filenames"); 
      system "rm $test_path/$ARGV[1]\.compare\n" if (-e "$test_path/$ARGV[1]\.compare"); 
      
      
      if(!defined($ARGV[1])){
        print "ERROR - MUST ENTER A FILE NAME\n";
        system "cat README.testsuites_script\n";
        die;
      }

      $ENV{'RUN'} = $ARGV[1];
      $_ = $ARGV[1];

	  /([\d]{4})([\d]{2})Z([\d]{3})/;
      my $YYYY = $1;
      my $MM = $2;
      my $ZONE = $3;
    
      die "Year value is out of range [1996, 2040]" if($YYYY < 1996 || $YYYY > 2040);
      die "Month value is out of range [1, 12]" if($MM < 1 || $MM > 12);
      die "Zone value is out of range [1, 180]" if($ZONE < 1 || $ZONE > 180);

# *** define number of days in the month
#
      my $leapyear = $YYYY % 4;
      my @days_per_month = qw/ 31 28 31 30 31 30 31 31 30 31 30 31 /;
      $days_per_month[1] = 29 if($leapyear == 0);

      my $Hours = $days_per_month[$MM - 1] * 24;


      unless (open NAMEFILE, "> tfile") {die "Cannot open tfile: $!";}
      
      print NAMEFILE "$ZONE\n";
      print NAMEFILE "$Hours\n";
      close (NAMEFILE);

      unless (open FILENAMES, "> filenames") {die "Cannot open filenames: $!";}
      
      print FILENAMES "$ENV{'CERESHOME'}/sarb/data\_exp/CER7.2.1P2/$ARGV[0]\n";
#      print FILENAMES "$ENV{'CERESHOME'}/sarb/data/CERES/SYNI/Terra-Aqua-MODIS_Edition4Inputs/$YYYY/$MM/$ARGV[1]\n";
      print FILENAMES "$ENV{'CERESHOME'}/sarb/data/CERES/SYNI/$ENV{'SS7_2'}\_$ENV{'PS7_2'}\-$ARGV[2]/$YYYY/$MM/$ARGV[1]\n";
       print "$ENV{'CERESHOME'}/sarb/data\_exp/CER7.2.1P2/$ARGV[0]\n";
       print "$ENV{'CERESHOME'}/sarb/data/CERES/SYNI/$ENV{'SS7_2'}\_$ENV{'PS7_2'}\-$ARGV[2]/$YYYY/$MM/$ARGV[1]\n";
      close (FILENAMES);

      system "ln -s $ENV{'CERESHOME'}/sarb/CER7.2.1P2/test\_suites/tfile Input\n";

      system "$ENV{'CERESHOME'}/sarb/CER7.2.1P2/test_suites/CER7.2.1P2Comp_$hostlocal.exe\n";
 
      system "rm Input tfile filenames\n";
      system "mv Diag_Out $ENV{'CERESHOME'}/sarb/CER7.2.1P2/test\_suites/$ARGV[1]\.compare\n";
#
#
