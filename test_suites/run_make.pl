#!/usr/bin/env perl
#$ -S /usr/bin/

use strict;

my $hostlocal = "x86_64";

if (defined $ARGV[0]){
 system "make -f Makefile_$hostlocal clean\n";
 system "make -f Makefile_$hostlocal\n";
}
else{
 system "make -f Makefile_$hostlocal\n";
}
	  		

 
	
 
  
   
