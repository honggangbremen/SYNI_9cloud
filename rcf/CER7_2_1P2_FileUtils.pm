package CER7_2_1P2_FileUtils;
#
#
########################
#
#
# Name:
#   CER7_2_1P2_FileUtils.pm
#
# Description:
#
#  Purpose - This script determines the input and output product file names
#            that will be needed for the PGE.  These input and output products
#            determined here are used for generating the PCF and the PCF log
#            file.
#
# Input Parameters:
#    none
#   
# Revision History:
#   Version 2.0    October 5, 2011
#                  Nelson Hillyer, SSAI, thomas.n.hillyer@nasa.gov
#                  Standardized input/output subroutine arguments and return values for use
#                  with the AJSS_PGE framework.
#   Version 1.2    T. Nelson Hillyer, SSAI (thomas.n.hillyer@nasa.gov)
#                  September 28, 2011
#                  Changed the ECS, ESNOW, and EICE input data products to be
#                  flagged as a "Mandatory" input.
#   Version 1.1    T. Nelson Hillyer, SSAI (thomas.n.hillyer@nasa.gov)
#                  August 2, 2011
#                  Fixed error in the way that Get_Days_In_Month is being called
#                  which would cause the wrong number of days to be given for a
#                  month in certain cases.
#
################################################################################
use Exporter;
use Toolkit::PCF;
use CERES::PCFChecks qw(checkIsMissing);
use strict;

our @ISA = qw(Exporter);
our @EXPORT_OK = qw(checkInput);

sub checkInput {
	my %args = @_;
	
	my $pcf = $args{'pcf'};
	
	my $missing_input = CERES::PCFChecks::checkForMissingEssentials(pcf => $pcf);
	
	my %inputFiles = $pcf->getInputFiles();
	my %supportInputFiles = $pcf->getSupportInputFiles();
	%inputFiles = (%inputFiles, %supportInputFiles);
	
	for my $lid (keys %inputFiles) {
		for my $entry (@{$inputFiles{$lid}}) {
			#print $entry->{file}."\n";
			if ($entry->{class} eq 'Mandatory' && checkIsMissing(item => $entry)) {
				$missing_input++;
			}
		}
	}
	
	return $missing_input;
}

1;
