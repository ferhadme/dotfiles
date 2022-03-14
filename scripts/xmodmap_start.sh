#!/usr/bin/bash

### Taken from: https://askubuntu.com/questions/7190/why-wont-my-xmodmap-file-load-on-login/967527#967527

sleep 5
/usr/bin/xmodmap /home/ferhad/.Xmodmap &
date >> /tmp/xmodmap-has-run
