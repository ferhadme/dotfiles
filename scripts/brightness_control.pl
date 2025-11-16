#!/usr/bin/perl

=about

Author: Farhad Mehdizada (@ferhadme)

This script is for controlling brightness of my computer

Usage: ./brightness_control option
Options:
  --get
  --set <value>
  --inc <value>
  --dec <value>
  --help

Note: You should run script with sudo privileges because of file location

=cut

use strict;
use warnings;
use v5.32;

use constant {
    MIN_BRIGHTNESS => 0, # blank screen

    # In newer kernels, the AMDGPU driver switched to using the
    # firmware’s native hardware scale for backlight, which can be much higher precision than old kernels (0-255)
    DEFAULT_BRIGHTNESS => 52000
};

use constant BRIGHTNESS_FILELOCS => [
    '/sys/class/backlight/amdgpu_bl0/brightness',
    '/sys/class/backlight/amdgpu_bl1/brightness',
    '/sys/class/backlight/amdgpu_bl2/brightness'
];

use constant MAX_BRIGHTNESS_FILELOCS => [
    '/sys/class/backlight/amdgpu_bl0/max_brightness',
    '/sys/class/backlight/amdgpu_bl1/max_brightness',
    '/sys/class/backlight/amdgpu_bl2/max_brightness'
];

my $max_brightness;
my $brightness_fileloc;
my $max_brightness_fileloc;

my %opt = qw /
   --help --help
   --get --get
   --set --set
   --inc --inc
   --dec --dec
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

    $brightness_fileloc = (grep { -e $_ } @{BRIGHTNESS_FILELOCS()})[0];
    my $current_brightness = &get_brightness_value_from($brightness_fileloc);

    if ($option eq '--get') {
        print $current_brightness;
        exit;
    }

    if ($option eq '--default') {
        &set_brightness_value(DEFAULT_BRIGHTNESS);
        exit;
    }

    &init_max_brightness;
    my $value = &get_value;
    if ($option eq '--set') {
        &set_brightness_value($value);
        exit;
    }

    if ($option eq '--inc') {
        my $inc_brightness = $current_brightness + $value;
        if ($inc_brightness > $max_brightness) {
            &set_brightness_value($max_brightness);
            exit;
        }
        &set_brightness_value($inc_brightness);
    }

    if ($option eq '--dec') {
        my $dec_brightness = $current_brightness - $value;
        if ($dec_brightness < MIN_BRIGHTNESS) {
            &set_brightness_value(MIN_BRIGHTNESS);
            exit;
        }
        &set_brightness_value($dec_brightness);
    }
}

sub init_max_brightness {
    $max_brightness_fileloc = (grep { -e $_ } @{MAX_BRIGHTNESS_FILELOCS()})[0];
    $max_brightness = &get_brightness_value_from($max_brightness_fileloc);
}

sub get_brightness_value_from {
    ($_) = (@_);
    open(IN, '<', $_) or die $!;
    my $val = <IN>;
    close(IN);
    $val;
}

sub set_brightness_value {
    ($_) = (@_);
    open(OUT, '>', $brightness_fileloc) or die $!;
    print OUT $_;
    close(OUT);
}

sub get_value {
    $_ = shift @ARGV or die &usage;
    (return $_) if (/^\d+$/ and $_ >= MIN_BRIGHTNESS and $_ <= $max_brightness)
        or die "Brightness value is invalid\n";
}

sub usage {
    "Usage: ./brightness_control option\n" .
        "Options:\n" .
        "\t--get\n" .
        "\t--set <value>\n" .
        "\t--inc <value>\n" .
        "\t--dec <value>\n" .
        "\t--default\n" .
        "\t--help\n" .
        "\n" .
        "Note: You should run script with sudo privileges because of file location\n";
}

main;
exit;
