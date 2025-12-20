#!/usr/bin/bash

i3lock -i ~/Pictures/arch.png -t &&
    sh -c "echo mem > /sys/power/state"
