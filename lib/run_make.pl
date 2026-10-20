#!/usr/bin/env perl
#$ -S /usr/bin/

use strict;

my $hostlocal = "$ENV{'CPUTYPE'}";

if ($ENV{'CPUTYPE'} eq 'x86_64-linux'){
 $hostlocal = "x86_64";}
if ($ENV{'CPUTYPE'} eq 'ppc64-linux'){
 $hostlocal = "ppc64";}

if (defined $ARGV[0]){
 system "make -f Makefile_$hostlocal clean\n";
 system "make -f Makefile_$hostlocal\n";
}
else{
 system "make -f Makefile_$hostlocal\n";
}


	  		

 
	
 
  
   
