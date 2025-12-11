#!/bin/bash

brightness=""
last_update=0

i3status "$@" | while read -r line
do
    now=$(date +%s)
    if [ $((now - last_update)) -ge 2 ]; then
        brightness=$(b-control --getp)
        last_update=$now
    fi
    echo "☀️: $brightness % | $line" || exit 1
done
