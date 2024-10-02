#!/usr/bin/bash

i3lock -i ~/Pictures/Wallpapers/lockscreen.png -f -t &&
    sh -c "echo mem > /sys/power/state"
