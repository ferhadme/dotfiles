#!/usr/bin/bash

i3lock -i ~/dotfiles/wallpapers/wallpaper.png -t &&
    sh -c "echo mem > /sys/power/state"
