#!/usr/bin/bash

set -e

cd ~

git clone https://github.com/ferhadme/dotfiles.git

ln -sfv ~/dotfiles/.vanilla.emacs.d ~/.emacs.d

ln -sfv ~/dotfiles/.vimrc ~/.vimrc

ln -sfv ~/dotfiles/.ideavimrc ~/.ideavimrc

ln -sfv ~/dotfiles/i3 ~/.config/i3

ln -sfv ~/dotfiles/i3status ~/.config/i3status

ln -sfv ~/dotfiles/polybar ~/.config/polybar

ln -sfv ~/dotfiles/picom ~/.config/picom

ln -sfv ~/dotfiles/alacritty ~/.config/alacritty

ln -sfv ~/dotfiles/dunst ~/.config/dunst

ln -sfv ~/dotfiles/.bashrc ~/.bashrc

ln -sfv ~/dotfiles/.tmux.conf ~/.tmux.conf

ln -sfv ~/dotfiles/.gitconfig ~/.gitconfig

ln -sfv ~/dotfiles/.mongoshrc.js ~/.mongoshrc.js

ln -sfv ~/dotfiles/.psqlrc ~/.psqlrc

ln -sfv ~/dotfiles/.xprofile ~/.xprofile
ln -sfv ~/dotfiles/.xinitrc ~/.xinitrc

sudo ln -s ~/dotfiles/scripts/fan_control.pl /usr/local/bin/f-control
sudo ln -s ~/dotfiles/scripts/brightness_control.pl /usr/local/bin/b-control
sudo ln -s ~/dotfiles/scripts/suspend-i3.sh /usr/local/bin/suspend-i3

sudo ln -sfv ~/dotfiles/ly/config.ini /etc/ly/config.ini
