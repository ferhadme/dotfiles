#!/usr/bin/bash

i3lock &&
    sh -c "echo mem > /sys/power/state"
