#!/usr/bin/perl

=about

Author: Farhad Mehdizada (@ferhadme)

This script is for controlling fan mode of asus gaming laptops on Linux

Be sure you use kernel version 5.6+

Usage: sudo ./fan_control option
Options:
  {silent|balance|turbo}
  status
  --help
Note: You should run script with sudo privileges because of file location

=cut

use strict;
use warnings;
use v5.32;

use constant {
    FILELOC => '/sys/devices/platform/asus-nb-wmi/throttle_thermal_policy',
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

    if ($option eq '--help') {
        &print_usage;
        exit;
    }

    if (not -e FILELOC) {
        print FILELOC, " not exists\n";
        print "Be sure you use Asus Gaming laptop and have Kernel version 5.6+\n";
        exit;
    }

    my $current_fan_mode = &status;
    
    if ($option eq 'silent') {
        change_fan_mode(SILENT);
        change_status_log($current_fan_mode, 'silent');
    } elsif ($option eq 'balance') {
        change_fan_mode(BALANCE);
        change_status_log($current_fan_mode, 'balance');
    } elsif ($option eq 'turbo') {
        change_fan_mode(TURBO);
        change_status_log($current_fan_mode, 'turbo');
    } elsif ($option eq 'status') {
        print "Current fan mode is ${current_fan_mode}\n";
    } else {
        print "fan_control: invalid argument\n";
        &print_usage;
    }
}

sub change_status_log {
    my ($prev_fan_mode, $current_fan_mode) = @_;
    print "Fan mode has been changed from $prev_fan_mode to $current_fan_mode\n";
}

sub change_fan_mode {
    my $fan_mode = shift @_;
    open(FILE, '>', FILELOC) or die $!;
    print FILE $fan_mode;
    close(FILE);
}

sub status {
    my %fan_modes = ( SILENT => 'silent',
                      BALANCE => 'balance',
                      TURBO => 'turbo' );

    open(FILE, '<', FILELOC) or die $!;
    my $current_mode = <FILE>;
    if ($current_mode == SILENT) {
        return "silent";
    } elsif ($current_mode == BALANCE) {
        return "balance";
    } elsif ($current_mode == TURBO) {
        return "turbo";
    }
    return "undefined";
}

sub print_usage {
    print "Usage: sudo ./fan_control option\n";
    print "Options:\n";
    print "\t{silent|balance|turbo}\n";
    print "\tstatus\n";
    print "\t--help\n";
    print "Note: You should run script with sudo privileges because of file location\n";
}

main;
exit;
