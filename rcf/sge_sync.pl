#!/usr/bin/perl
use IPC::Open2;
use strict;
use warnings;

my ($chld_out, $chld_in);

my $pid = open2($chld_out, $chld_in, @ARGV, '2>&1');

my %running_ids;
my %completed_ids;

while ( my $line = <$chld_out> ) {
    chomp $line;

    if ( $line =~ /^Your job (\d+)/ ) {
        $running_ids{$1} = 0;
    }

    print $line, "\n";
}

waitpid($pid, 0);

my $driver_exit = $?;

exit 1 unless ( scalar %running_ids );

print "\n";
#print "the following SGE job ids were submitted:\n";
for my $id ( sort { $a <=> $b } keys %running_ids ) {
    print "waiting for job $id to complete...\n";
}

#print "waiting for jobs to complete...\n";
sleep(15);

while ( 1 ) {
    for my $id ( keys %running_ids ) {
        system("qstat -j $id 2>/dev/null >/dev/null");

        if ( $? != 0 ) {
            $completed_ids{$id} = 0;
            delete $running_ids{$id};
        }
    }

    last unless ( scalar %running_ids );
    sleep(10);
}
    
print "job finished\n";
print "giving accounting file time to be written...\n";
sleep(10);

my $jobs_failed = 0;
for my $id ( sort { $a <=> $b } keys %completed_ids ) {
#    print "checking $id\n";
    my @lines = `qacct -j $id`;
    chomp @lines;

    my ($failed, $exit_status);

    for my $line ( @lines ) {
        if ( $line =~ /^exit_status\s+(\d+)/ ) {
            $exit_status = $1;
        }
        elsif ( $line =~ /^failed\s+(\d+)/ ) {
            $failed = $1;
        }
    }

    die "unable to qacct correctly" unless ( defined $failed && defined $exit_status);

    if ( $failed != 0 || $exit_status != 0 ) {
        print "$id failed with sge_failure=$failed,exit_status=$exit_status\n";
        $jobs_failed++;
    }
}

print "\n";

if ( $jobs_failed ) {
    print "$jobs_failed jobs failed!\n";
    exit 1;
}
else {
#    print "no jobs failed!\n";
    exit 0;
}

