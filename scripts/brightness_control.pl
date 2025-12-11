#!/usr/bin/perl

=about

Author: Farhad Mehdizada (@ferhadme)

This script is for controlling brightness of my computer

Usage: ./brightness_control option <arg>
Options:
   --help        Prints this message
   --get         Gets current brightness value
   --getp        Gets current brightness percentage
   --getmax      Gets maximum brightness value
   --default     Sets brightness value to default value (90%)
   --set  <arg>  Sets specified brightness value
   --setp <arg>  Sets specified brightness percentage
   --inc  <arg>  Increments brightness value by specified value
   --incp <arg>  Increments brightness percentage by specified value
   --dec  <arg>  Decrements brightness value by specified value
   --decp <arg>  Decrements brightness percentage by specified value

Note: You should run script with sudo privileges because of file location

=cut

use strict;
use warnings;
use v5.32;

use List::Util qw(max);
use List::Util qw(min);

use constant {
    MIN_BRIGHTNESS => 0, # blank screen

    DEFAULT_BRIGHTNESS_P => 85,
    MAX_BRIGHTNESS_P => 100,

    BRIGHTNESS_FILELOC => '/sys/class/backlight/*/brightness',
    MAX_BRIGHTNESS_FILELOC => '/sys/class/backlight/*/max_brightness',
};

my $max_brightness;
my $brightness_fileloc;

my %opt = qw /
   --help --help
   --get --get
   --getp --getp
   --getmax --getmax
   --set --set
   --setp --setp
   --inc --inc
   --incp --incp
   --dec --dec
   --decp --decp
   --default --default
/;

sub main {
    my $option = shift @ARGV or die &usage;
    if (not defined $opt{$option}) {
        die &usage;
    }

    if ($option eq '--help') {
        print &usage;
        exit;
    }

    ($brightness_fileloc) = glob(BRIGHTNESS_FILELOC);
    my $current_brightness = &get_brightness_value_from($brightness_fileloc);

    if ($option eq '--get') {
        print $current_brightness;
        exit;
    }

    &init_max_brightness;

    if ($option eq '--getp') {
        print &get_brightness_as_perc($current_brightness) . "\n";
        exit;
    }

    if ($option eq '--getmax') {
        print $max_brightness;
        exit;
    }

    if ($option eq '--default') {
        my $brightness_value = &get_brightness_as_value(DEFAULT_BRIGHTNESS_P);

        &set_brightness($brightness_value);
        exit;
    }

    my $arg_value = &get_arg_as_value;

    if ($option eq '--set') {
        &set_brightness($arg_value);
        exit;
    }

    if ($option eq '--setp') {
        my $brightness_value = &get_brightness_as_value($arg_value);

        &set_brightness($brightness_value);
        exit;
    }

    if ($option eq '--inc') {
        my $inc_brightness = min($current_brightness + $arg_value, $max_brightness);

        &set_brightness($inc_brightness);
        exit;
    }

    if ($option eq '--incp') {
        my $brightness_perc = min(&get_brightness_as_perc($current_brightness) + $arg_value, MAX_BRIGHTNESS_P);
        my $inc_brightness = &get_brightness_as_value($brightness_perc);

        &set_brightness($inc_brightness);
        exit;
    }

    if ($option eq '--dec') {
        my $dec_brightness = max($current_brightness - $arg_value, MIN_BRIGHTNESS);

        &set_brightness($dec_brightness);
        exit;
    }

    if ($option eq '--decp') {
        my $brightness_perc = max(&get_brightness_as_perc($current_brightness) - $arg_value, MIN_BRIGHTNESS);
        my $dec_brightness = &get_brightness_as_value($brightness_perc);

        &set_brightness($dec_brightness);
        exit;
    }
}

sub init_max_brightness {
    my ($max_brightness_fileloc) = glob(MAX_BRIGHTNESS_FILELOC);
    $max_brightness = &get_brightness_value_from($max_brightness_fileloc);
}

sub get_brightness_value_from {
    ($_) = (@_);
    open(IN, '<', $_) or die $!;
    my $val = <IN>;
    close(IN);
    $val;
}

sub set_brightness {
    ($_) = (@_);
    open(OUT, '>', $brightness_fileloc) or die $!;
    print OUT $_;
    close(OUT);
}

sub get_arg_as_value {
    $_ = shift @ARGV or die &usage;
    (return $_) if (/^\d+$/ and $_ >= MIN_BRIGHTNESS and $_ <= $max_brightness)
        or die "Brightness value is invalid\n";
}

sub get_brightness_as_perc {
    ($_) = (@_);
    int(($_ * 100) / $max_brightness);
}

sub get_brightness_as_value {
    ($_) = (@_);
    int(($max_brightness * $_) / 100) if $_ >= MIN_BRIGHTNESS and $_ <= MAX_BRIGHTNESS_P
        or die "Brightness percentage is invalid\n";
}

sub usage {
    "Usage: ./brightness_control option <arg>\n" .
        "Options:\n" .
        "--help        Prints this message\n" .
        "--get         Gets current brightness value\n" .
        "--getp        Gets current brightness percentage\n" .
        "--getmax      Gets maximum brightness value\n" .
        "--default     Sets brightness value to default value (90%)\n" .
        "--set  <arg>  Sets specified brightness value\n" .
        "--setp <arg>  Sets specified brightness percentage\n" .
        "--inc  <arg>  Increments brightness value by specified value\n" .
        "--incp <arg>  Increments brightness percentage by specified value\n" .
        "--dec  <arg>  Decrements brightness value by specified value\n" .
        "--decp <arg>  Decrements brightness percentage by specified value\n" .
        "Note: You should run script with sudo privileges because of file location\n";
}

main;
exit;
