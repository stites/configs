export TERM="screen-256color"
export EDITOR='nvim'
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
set +o vi

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# ========================================================= #
# history and autofill                                      #
# ========================================================= #

# don't put duplicate lines in the history. See bash(1) for more options
# ... or force ignoredups and ignorespace
export HISTCONTROL=ignoredups:ignorespace:erasedups

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=1000

# shopt -s histappend     # append to the history file, don't overwrite it
shopt -s nocaseglob     # auto corrects the case
# shopt -s checkwinsize   # check the window size after each command and, if
                        # necessary, update the values of LINES and COLUMNS.

# bash automatically fetches the last command that starts with the
# given term: E.G. you type in ‘ssh’ and press the ‘Page Up’ key and bash
# scrolls through your history for this. Store function in .inputrc
export INPUTRC=$HOME/.inputrc

# ========================================================= #
# ls config                                                 #
# ========================================================= #
# some ls aliases
alias tree='tree -C'
alias ls='ls -G --color'

# Relative Jumps:
alias ~='cd ~ '
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias .....='cd ../../../..'

# ========================================================= #
# initialization scripts which are auto-generated           #
# ========================================================= #
# disabling for speed - you might have to run these on startx
# alias start_irssi='bitlebee -F -u stites && irssi'
# alias r='grunt'

#export NODE_PATH=/usr/local/lib/node:/usr/local/lib/node_modules:$HOME/.nvm/v0.10.32/lib/node_modules

# ===================== #
# .bashrc functions     #
# ===================== #
alias vrc='vim ~/.bashrc'
alias src='source ~/.bashrc'
# == ghci to bash == #
alias ":q"=exit
alias ":r"=myReload

#=======================#
# Add git-aware prompt  #
#=======================#
# -- primarily cause I'm super lazy: https://github.com/jimeh/git-aware-prompt
export GITAWAREPROMPT=~/.bash/git-aware-prompt
if [ -e $GITAWAREPROMPT ]; then
  source "${GITAWAREPROMPT}/main.sh"
fi

#=======================#
# Use Nix               #
#=======================#
if [ -e $HOME/.nix-profile/etc/profile.d/nix.sh ]; then
  . $HOME/.nix-profile/etc/profile.d/nix.sh;
fi

# ========================================= #
# TODO: load init configs in plist somehow  #
# ========================================= #
[[ ! -f ~/git/cfg/init.d/load_env  ]] || source ~/git/cfg/init.d/load_env

# ========================================= #
# Load the remaining settings               #
# ========================================= #

for SETTING in java git bina tmux mac emacs task nginx vim python el-captain; do
  [[ ! -f ~/.bashrc_$SETTING  ]] || source ~/.bashrc_$SETTING
done

[[ ! -f ~/.bash-wakatime/bash-wakatime.sh  ]] || source ~/.bash-wakatime/bash-wakatime.sh

# add stack installs to path
safe_path_add ~/.local/bin/

# stack autocomplete
eval "$(stack --bash-completion-script stack)"

# ========================================= #
# write a note                              #
# ========================================= #
function stacknew {
  stack new $1 --bare ~/git/stack-templates/skeleton
}
