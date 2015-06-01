[[ ! -f  ~/.bash_colors  ]] || source ~/.bash_colors

export TERM="screen-256color"
export EDITOR='vim'
set +o vi

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# ========================================================= #
# history and autofill                                      #
# ========================================================= #

# don't put duplicate lines in the history. See bash(1) for more options
# ... or force ignoredups and ignorespace
HISTCONTROL=ignoredups:ignorespace

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=1000

shopt -s histappend     # append to the history file, don't overwrite it
shopt -s nocaseglob     # auto corrects the case
shopt -s checkwinsize   # check the window size after each command and, if
                        # necessary, update the values of LINES and COLUMNS.

# bash automatically fetches the last command that starts with the
# given term: E.G. you type in ‘ssh’ and press the ‘Page Up’ key and bash
# scrolls through your history for this. Store function in .inputrc
export INPUTRC=$HOME/.inputrc

function h {
  if [[ "$1" =~ "^\d+$" ]]; then
    !$1
  elif [ "$1" != "" ]; then
    history | grep $1
  else
    history
  fi
}

# ========================================================= #
# ls config                                                 #
# ========================================================= #

# some more ls aliases
alias grp='grep -RiI'
alias tree='tree -C'
#alias ls='ls -G --color'
#function ls {
#  DIR=$( pwd )
#  if [ -e "${DIR}/$1" ]; then
#    ls -G $1
#  elif [ "$1" != "" ]; then
#    ls -halpGF | grep $1
#    echo "--> grep $1"
#  else
#    ls -halpGF
#  fi
#}


function l {
  if [ "$2" != "" ]; then
    ls -halGpF $1 | grep $2
    echo "--> grep $2"
  elif [ -e "$1" ]; then
    ls -halGpF $1
  elif [ "$1" != "" ]; then
    ls -halGpF | grep $1
    echo "--> grep $1"
  else
    ls -halGpF
  fi
}


# ========================================================= #
# vagrantbox stuff                                          #
# ========================================================= #

alias cdv='cd ~/vagrant'
alias vssh='vagrant ssh'
alias vup='vagrant up'
alias vhalt='vagrant halt'
alias vboot='vagrant up; vagrant ssh'

# ========================================================= #
# Load Environments                                         #
# ========================================================= #

# ========================================================= #
# PATH Exports                                              #
# ========================================================= #
function safe_path_add {
  PATH_FOUND=$(echo $PATH | grep -o -E "(^|:)$1" | head -1)

  if [ -z $PATH_FOUND ] && [ -d $1 ]; then
    export PATH=$PATH:$1
  # else
  #    test -z $PATH_FOUND && echo "$1 not a directory" || echo "$1 in PATH"
  fi
}
export -f safe_path_add

# nvm, rvm
for ENV_MGR in .nvm .rvm; do
  test ! -d $HOME/$ENV_MGR && echo "$ENV_MGR not detected" && continue;
  find -P  $HOME/$ENV_MGR -type d -maxdepth 3 -name bin | xargs -n 1 bash -c 'safe_path_add "$@"' _
done

# pyenv
safe_path_add $HOME/.pyenv/bin
safe_path_add $HOME/.pyenv/shims

for BIN_PATH in '' /usr /usr/local; do
  for BIN_TYPE in /bin /sbin; do
    safe_path_add $BIN_PATH$BIN_TYPE
  done
done

# ========================================================= #
# vim, bash editing, terminal settings, relative jumps      #
# ========================================================= #

# rename tabs:
function t {
  printf "\e]1;$1\a"
}
function winname {
  printf "\e]2;$1\a"
}


# Relative Jumps:
alias ~='cd ~ '
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias .....='cd ../../../..'

# ========================================================= #
# initialization scripts which are auto-generated           #
# ========================================================= #
eval "$(fasd --init auto)"
alias start_irssi='bitlebee -F -u stites && irssi'

alias r='grunt'
alias t='clear && task'

export NODE_PATH=/usr/local/lib/node:/usr/local/lib/node_modules:$HOME/.nvm/v0.10.32/lib/node_modules

# ===================== #
# .bashrc functions     #
# ===================== #
alias vrc='vim ~/.bashrc'
alias src='source ~/.bashrc'

function addrc {
  echo $1 >> ~/.bashrc
  src
}

if [ -f $(brew --prefix)/etc/bash_completion ]; then
  . $(brew --prefix)/etc/bash_completion
fi

# ========================================= #
# Load the remaining settings               #
# ========================================= #
for SETTING in kafka git bina tmux npm python arch gauss mac jira emacs mu java ssh task spark ruby; do
  [[ ! -f ~/.bashrc_$SETTING  ]] || source ~/.bashrc_$SETTING
done
