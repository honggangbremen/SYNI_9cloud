#!/usr/bin/env perl
#
#######################
#
#
# Name:
#   compile_7.2.1P2.pl
#
# Description:
#
#  Purpose - To generate the  executable used by Synoptic SARB Subsystem.
#
# Input Parameters:
#   None
#
#
# Revision History:
#  Version 3.0   October 15, 1999 - SCCR#
# 		 Thomas E. Caldwell, SAIC, t.e.caldwell@larc.nasa.gov
#		 Initial version of code
#######################

	my $Option;

    if(!defined($ARGV[0]))
    {
	 print "Choose code to compile\n";
	 print "(fulib, lib, src, all, clean)\n";
	 chomp($Option = <STDIN>);
    }
    else{
     $Option = $ARGV[0] 
    }
#
# Prepare message files
#
	if ($Option eq "smf" or $Option eq "all"){
	my $smf_dir = "$ENV{'CERESHOME'}/sarb/smf/sarb";
	chdir "$smf_dir"    or die "cannot chdir to $smf_dir: $!";
	system "./smfcompile_all.csh";}
#
# Create FUlib
#
	if ($Option eq "fulib" or $Option eq "all"){
        my $lib_dir = "$ENV{'CERESHOME'}/sarb/CER7.2.1P2/lib/fulib";
	chdir "$lib_dir"    or die "cannot chdir to $lib_dir: $!";
        system "./run_make.pl clean";}
#
# Create SARBlib
#	
	if ($Option eq "lib" or $Option eq "all"){
        my $lib_dir = "$ENV{'CERESHOME'}/sarb/CER7.2.1P2/lib";
	chdir "$lib_dir"    or die "cannot chdir to $lib_dir: $!";
        system "./run_make.pl clean";}
#
# Create SYN executable
#
	if ($Option eq "src" or $Option eq "all"){
	my $src_dir = "$ENV{'CERESHOME'}/sarb/CER7.2.1P2/src";
	chdir "$src_dir"    or die "cannot chdir to $src_dir: $!";
        system "./run_make.pl clean";}
#
# Create comparison software executable
#
	if ($Option eq "test" or $Option eq "all"){
	my $src_dir = "$ENV{'CERESHOME'}/sarb/CER7.2.1P2/test_suites";
	chdir "$src_dir"    or die "cannot chdir to $src_dir: $!";
        system "./run_make.pl clean";}
#
# Clean directories
#
    my $make_cmd;
    if ($ENV{"BRAND"} eq "linux"){ 
     $make_cmd = "make";}
    else{ 
     $make_cmd = "make";}

     my $hostlocal = "x86_64";

	if ($Option eq "clean"){
         $make_cmd = "make clean -f Makefile_$hostlocal";
	 my $action;
	 print "Choose code to clean\n";
	 print "(fulib, lib, src, test, all)\n";
	 chomp($action = <STDIN>);
	 
	 
	 if ($action eq "fulib" or $action eq "all") {
          my $lib_dir = "$ENV{'CERESHOME'}/sarb/CER7.2.1P2/lib/fulib";
	  chdir "$lib_dir"    or die "cannot chdir to $lib_dir: $!";
          system "$make_cmd";
	  }
	 if ($action eq "lib" or $action eq "all") {
          my $lib_dir = "$ENV{'CERESHOME'}/sarb/CER7.2.1P2/lib";
	  chdir "$lib_dir"    or die "cannot chdir to $lib_dir: $!";
          system "$make_cmd";
	  }	  
	  
	 if ($action eq "src" or $action eq "all") {
          my $src_dir = "$ENV{'CERESHOME'}/sarb/CER7.2.1P2/src";
	  chdir "$src_dir"    or die "cannot chdir to $src_dir: $!";
          system "$make_cmd";
	  }
	  
	  
	 if ($action eq "test" or $action eq "all") {
          my $src_dir = "$ENV{'CERESHOME'}/sarb/CER7.2.1P2/test_suites";
	  chdir "$src_dir"    or die "cannot chdir to $src_dir: $!";
          system "$make_cmd";
	  }
     }

