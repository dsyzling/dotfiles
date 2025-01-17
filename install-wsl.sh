#!/bin/bash
stow wsl-zsh
stow git
stow emacs
stow fonts
stow xorg
stow tmux

# Install oh-my-zsh
sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"

tic -x -o ~/.terminfo wsl-zsh/terminfo-24bit.src
