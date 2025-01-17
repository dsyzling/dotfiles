#!/bin/bash
# Install oh-my-zsh
sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
mv ~/.zshrc ~/.zshrc-old
stow wsl-zsh
stow git
stow emacs
stow fonts
stow xorg
stow tmux

tic -x -o ~/.terminfo wsl-zsh/terminfo-24bit.src
