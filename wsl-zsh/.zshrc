# start tmux automatically - this causes issues with Emacs
# so we start Tilix with -e tmux to auto start tmux on our terminal app.
#export ZSH_TMUX_AUTOSTART=true

# Path to your oh-my-zsh installation.
export ZSH="/home/dsyzling/.oh-my-zsh"

# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
ZSH_THEME="robbyrussell"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Which plugins would you like to load?
# Standard plugins can be found in ~/.oh-my-zsh/plugins/*
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(
    git tmux ssh-agent
)

source $ZSH/oh-my-zsh.sh

# User configuration
# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# opt out of dotnet telemetry being sent to MS
export DOTNET_CLI_TELEMETRY_OPTOUT=true

export DISPLAY=$(ip route | awk '{print $3; exit}'):0
export LIBGL_ALWAYS_INDIRECT=1

export TERM=xterm-24bits

# Allow colour support within Emacs shell.
if [ "$TERM" = dumb ] && [ "$INSIDE_EMACS" ]; then
    export TERM=xterm-256color
fi

# Testing removing Windows paths to speed up Emacs.
export PATH="/home/dsyzling/.local/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games"

# Scale GTK apps on hidpi screens - on ubuntu 20.x
# this has to be an integer based scaler, combine with
# -dide.ui.scale in JVM applications which can be a non integer
# value.
# Note - we do not need to use this when using the latest
# versions of WSL.
# export GDK_SCALE=2.0

# for WSL to use the internal Windows 11 Linux graphical
# support our display should be set to the following.
export DISPLAY=:0

# update keyboard repeat rate for x-windows
xset r rate 200 100

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# Allows tracing debugpy adapter from vscode connected to wsl
# export PYDEVD_DEBUG=true
# export DEBUGPY_LOG_DIR="/home/dsyzling/dev/vscodelog"
# export PYDEVD_DEBUG_FILE="/home/dsyzling/dev/vscodelog"

# hook direnv into shell
eval "$(direnv hook zsh)"

# Issue with WSL and Ubuntu falling back to using X11 rather than Wayland.
# Wayland will provide the smoother fonts on hidpi displays
#   https://github.com/microsoft/wslg/issues/1244
ln -sf //mnt/wslg/runtime-dir/wayland-* $XDG_RUNTIME_DIR//
