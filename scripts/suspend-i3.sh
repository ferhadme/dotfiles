#!/usr/bin/bash

i3lock -i ~/Pictures/arch.png -t &&
    sudo sh -c "echo mem > /sys/power/state"
