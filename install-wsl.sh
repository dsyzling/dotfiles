#!/bin/bash
stow wsl-zsh
stow git
stow emacs
stow fonts
stow xorg
stow tmux

tic -x -o ~/.terminfo wsl-zsh/terminfo-24bit.src
