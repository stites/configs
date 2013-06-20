# ========================================================= #
# .bashrc for stitesSR                                      #
# Copied here in the README for ease of access              #
# ========================================================= #

# ~/.bashrc: executed by bash(1) for non-login shells.  # see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# ========================================================= #
# beautify the terminal                                     #
# ========================================================= #

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
        # We have color support; assume it's compliant with Ecma-48
        # (ISO/IEC-6429). (Lack of such support is extremely rare, and such
        # a case would tend to support setf rather than setaf.)
        color_prompt=yes
    else
        color_prompt=
    fi
fi

if [ "$color_prompt" = yes ]; then
    PS1="\e[32;1m[\u@\[\e[35;1m\]\H \[\e[0m\]\W]\$ "
else
    PS1="[\u@\h:\W]\$ "
fi
unset color_prompt force_color_prompt

# ========================================================= #
# history and autofill                                      #
# ========================================================= #

# don't put duplicate lines in the history. See bash(1) for more options
# ... or force ignoredups and ignorespace
HISTCONTROL=ignoredups:ignorespace

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=2000
HISTFILESIZE=4000

shopt -s histappend     # append to the history file, don't overwrite it
shopt -s nocaseglob     # auto corrects the case
shopt -s checkwinsize   # check the window size after each command and, if 
                        # necessary, update the values of LINES and COLUMNS.

# bash automatically fetches the last command that starts with the
# given term: E.G. you type in ‘ssh’ and press the ‘Page Up’ key and bash
# scrolls through your history for this. Store function in .inputrc
export INPUTRC=$HOME/.inputrc 

# ========================================================= #
# ls config                                                 #
# ========================================================= #

# some more ls aliases
alias ll='ls -alF'
alias la='ls -lahL'
alias l='ls -CF'

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# ========================================================= #
# git config                                                #
# ========================================================= #

# git alias:
alias g='git'

# ========================================================= #
# ruby config                                               #
# ========================================================= #

# Add RVM to PATH for scripting:
PATH=$PATH:$HOME/.rvm/bin 

# ========================================================= #
# apt-get                                                   #
# ========================================================= #

# add alias':
alias i='sudo apt-get -y install'
alias r='sudo apt-get remove'
alias s='sudo apt-cache search'
alias u='sudo apt-get update'
alias rep='sudo add-apt-repository'
alias auto='sudo apt-get autoremove'
alias uu='sudo apt-get update & sudo apt-get upgrade'

# Relative Jumps
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias .....='cd ../../../..'

# ========================================================= #
# virtualenv, pip & python                                  #
# ========================================================= #

# Add alias for stats python env
alias pystats='source ~/Envs/stats/bin/activate'

# Add some pip shortcuts
alias pyi='pip install'
alias pysi='sudo pip install'

# And some ipython shortcuts
alias notebook='ipython notebook --pylab=inline'