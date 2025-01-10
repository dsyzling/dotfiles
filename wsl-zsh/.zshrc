# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

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

# Set list of themes to pick from when loading at random
# Setting this variable when ZSH_THEME=random will cause zsh to load
# a theme from this variable instead of looking in ~/.oh-my-zsh/themes/
# If set to an empty array, this variable will have no effect.
# ZSH_THEME_RANDOM_CANDIDATES=( "robbyrussell" "agnoster" )

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion.
# Case-sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# You can set one of the optional three formats:
# "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# or set a custom format using the strftime function format specifications,
# see 'man strftime' for details.
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

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

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/rsa_id"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

# pip should only run if there is a virtualenv currently activated
# Temporarily removing, so we can test pip inside conda.
#export PIP_REQUIRE_VIRTUALENV=true

# create commands to override pip restriction.
# use `gpip` or `gpip3` to force installation of
# a package in the global python environment
gpip(){
    PIP_REQUIRE_VIRTUALENV="" pip "$@"
}
gpip3(){
    PIP_REQUIRE_VIRTUALENV="" pip3 "$@"
}

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

# keychain and ssh-agent for caching git keys
# eval ``keychain --eval --agents ssh id_rsa
# source ~/.keychain/$HOSTNAME-sh

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
# xset r rate 200 250
xset r rate 200 100

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# Allows tracing debugpy adapter from vscode connected to wsl
# export PYDEVD_DEBUG=true
# export DEBUGPY_LOG_DIR="/home/dsyzling/dev/vscodelog"
# export PYDEVD_DEBUG_FILE="/home/dsyzling/dev/vscodelog"

# # prompt for venv with python
# setopt PROMPT_SUBST

# show_virtual_env() {
#   if [[ -n "$VIRTUAL_ENV" && -n "$DIRENV_DIR" ]]; then
#     echo "($(basename $VIRTUAL_ENV))"
#   fi
# }

# to support conda as well as venv.
show_virtual_env() {
  if [[ $(pyenv local 2>/dev/null) == *"conda"* ]]; then
     VENV=$CONDA_DEFAULT_ENV
  else
     VENV=$VIRTUAL_ENV
  fi
  if [[ -n "$VENV" && -n "$DIRENV_DIR" ]]; then
     echo "($(basename $VENV))"
  fi
}
PS1='$(show_virtual_env)'$PS1

# hook direnv into shell
eval "$(direnv hook zsh)"

# add uv to the path - added by installer.
. "$HOME/.cargo/env"
