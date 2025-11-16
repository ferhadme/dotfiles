#!/usr/bin/bash

set -e

cd ~

# git clone https://github.com/ferhadme/dotfiles.git

# Reset existing configurations
rm -rf ~/.config/i3 ~/.config/i3-status ~/.config/dunst ~/.tmux.conf ~/.bashrc ~/.emacs.d ~/.gitconfig ~/.vimrc

ln -sf ~/dotfiles/i3 ~/.config/i3
ln -sf ~/dotfiles/i3-status ~/.config/i3-status

ln -sf ~/dotfiles/dunst ~/.config/dunst

ln -sf ~/dotfiles/.tmux.conf ~/.tmux.conf

ln -sf ~/dotfiles/.bashrc ~/.bashrc

ln -sf ~/dotfiles/.vanilla.emacs.d ~/.emacs.d

ln -sf ~/dotfiles/.gitconfig ~/.gitconfig

ln -sf ~/dotfiles/.vimrc ~/.vimrc
