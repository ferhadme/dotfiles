#!/usr/bin/perl

=about

This script is for controlling fan mode of asus gaming laptops on Linux

Be sure you use kernel version 5.6+

Usage: sudo ./fan_control option
Options:
  {silent|balance|turbo}
  --help
Note: You should run script with sudo privileges because of file location

=cut

use strict;
use warnings;
use v5.32;

use constant {
    FILELOC => "/sys/devices/platform/asus-nb-wmi/throttle_thermal_policy",
    SILENT => 2,
    BALANCE => 0,
    TURBO => 1,
};

sub main {
    my $option = $ARGV[0];

    if (not defined $option) {
        print "fan_control: no fan mode specified\n";
        print "fan_control: Use --help for more information\n";
        exit;
    }

    if ($option eq "--help") {
        &print_usage;
        exit;
    }

    if (not -e FILELOC) {
        print FILELOC, " not exists\n";
        print "Be sure you use Asus Gaming laptop and have Kernel version 5.6+\n";
    }
    
    if ($option eq "silent") {
        change_fan_mode(SILENT);
    } elsif ($option eq "balance") {
        change_fan_mode(BALANCE);
    } elsif ($option eq "turbo") {
        change_fan_mode(TURBO);
    } else {
        print "fan_control: invalid argument\n";
        &print_usage;
    }
}

sub change_fan_mode {
    my $fan_mode = shift @_;
    open(FILE, '>', FILELOC) or die $!;
    print FILE $fan_mode;
    close(FILE);
}

sub print_usage {
    print "Usage: sudo ./fan_control option\n";
    print "Options:\n";
    print "\t{silent|balance|turbo}\n";
    print "\t--help\n";
    print "Note: You should run script with sudo privileges because of file location\n";
}

main;
exit;
