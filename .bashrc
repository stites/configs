export EDITOR='subl -w'

# ~/.bashrc: executed by bash(1) for non-login shells.  # see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# ========================================================= #
# beautify the terminal                                     #
# ========================================================= #

#HOSTNAME="MacAir"

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
    # At home, grey path
#   PS1="[\e[1;32m\u\[\e[1;35m\]@\H \[\e[1;30m\]\W\e[0m]\$ "
    PS1="\e[33;1m\]@\[\e[34;1m\]\W\e[35;1m\]~ \[\e[0m\]"
    # at home, white path
    #PS1="[\e[1;32m\u\[\e[1;35m\]@\H \[\e[0m\]\W]\$ "
    # In vagrant, grey path:
    # PS1="[\e[34;1m\u\[\e[0;34m\]@\H \[\e[1;30m\]\W\e[0m]\$ "

    # In AWS, white path:
    # PS1="[\e[0;33m\u\[\e[0;36m\]@aws \[\e[0m\]\W]\$ "
    # In AWS, grey path:
    # PS1="[\e[0;33m\u\[\e[0;36m\]@aws \[\e[1;30m\]\W\e[0m]\$ "

else
    PS1="[\u@\h:\W]\$ "
fi
unset color_prompt force_color_prompt

# Color   Code
# Black   0;30
# Blue    0;34
# Green   0;32
# Cyan    0;36
# Red     0;31
# Purple  0;35
# Brown   0;33
# replace 0 with 1 for a lighter version


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
alias l="ls -halGpF "
#alias l='ls -CF'
alias grp='grep -RiI'
alias ls='pwd && ls -G'
alias tree='tree -C'

# ========================================================= #
# git config                                                #
# ========================================================= #

# git alias:
alias gl="git log --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%ad @ %cr) %C(bold blue)<%an>%Creset' --graph --abbrev-commit --date=short"
alias g="git "
alias gfl="git fetch && gl"
alias gpl="git pull upstream master && gl"
alias go="git checkout "
alias gs='git status'
alias greset='git reset'
alias gmv="git mv "
alias gm="git merge "
alias gr="git rm "
alias ga="git add "
alias gc="git commit "
alias gclone="git clone "
alias gcm="gc -m "
alias grm='git rm '
alias gp='git push '
alias gpo='gp origin'
alias gph='gp heroku'
alias gpom='gpo master'
alias gphm='gph master'
alias gpog='gpo gh-pages'
alias gpub='go gh-pages && gpog && go master && gm gh-pages && gpom && go gh-pages && gs'
alias ghUp="gh pr --remote upstream -s"
alias gd="git diff"
alias stash="git stash"
alias pop="git stash pop"
alias gb='git branch'
alias gpull="git pull"
alias gpullup="gpull upstream master"

# ========================================================= #
# vagrantbox stuff                                          #
# ========================================================= #

alias cdv="cd ~/vagrant "
#alias cdv='cd /home/dbt/git/vagrant '
alias vboot='vagrant up; vagrant ssh'

# ========================================================= #
# kill processes                                           #
# ========================================================= #

alias killnode='killall -9 node'
alias killgrunt='killall -9 grunt'
alias killpsql='killall -9 psql'
alias killpostgres='killall -9 postgres'
alias exterminate='killall -9 node && killall -9 postgres && killall -9 psql && killall -9 grunt'

# ========================================================= #
# PATH Exports                                              #
# ========================================================= #

# add ruby paths for MACSTUFF!!!!
export PATH=$PATH:/usr/local/bin:/usr/local/share/npm/bin:/Users/stites/.rvm/gems/ruby-1.9.3-p448/bin:/Users/stites/.rvm/gems/ruby-1.9.3-p448@global/bin:/Users/stites/.rvm/rubies/ruby-1.9.3-p448/bin:/Users/stites/.rvm/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/Users/stites/.rvm/bin
export PATH=$PATH:/usr/local/sbin:/usr/local/share/npm/bin:/Users/stites/.rvm/gems/ruby-1.9.3-p448/bin:/Users/stites/.rvm/gems/ruby-1.9.3-p448@global/bin:/Users/stites/.rvm/rubies/ruby-1.9.3-p448/bin:/Users/stites/.rvm/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/Users/stites/.rvm/bin

# Add RVM to PATH for scripting:
PATH=$PATH:$HOME/.rvm/bin 
# do the same for node:
export PATH="/usr/local/share/npm/bin:$PATH"
export NODE_PATH="/usr/local/lib/node"

# Get Java 7 for Mac:
export JAVA_PATH="/Library/Internet\ Plug-Ins/JavaAppletPlugin.plugin/Contents/Home/bin"

# ========================================================= #
# vim, bash editing, terminal settings, relative jumps      #
# ========================================================= #

# vim:
alias v='vim '
alias vrc='vim ~/.bashrc'
alias src='source ~/.bashrc'

# rename tabs:
function tabname {
  printf "\e]1;$1\a"
}
function winname {
  printf "\e]2;$1\a"
}


# Relative Jumps:
#alias cdk='cd ~/Downloads/kaggle'
alias ~='cd ~ '
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias .....='cd ../../../..'

# terminal commands:
alias c="clear"
alias to="touch "
alias mk="mkdir "
alias mkp='mkdir -p '

# ========================================================= #
# Redis                                                     #
# ========================================================= #
alias redis-local="redis-server /usr/local/etc/redis.conf"

# ========================================================= #
# Postgres                                                  #
# ========================================================= #
alias pg="postgres -D /usr/local/var/postgres/"
alias psqlSEED="psql -h 127.0.0.1 -p 5432 -U stites -w seedchange"
alias psql-w="psql -h 127.0.0.1 -p 5432 -U stites -w "
alias psqlSEEDAWS="psql -h seedchangeim.c66sp35lkt8d.us-west-1.rds.amazonaws.com -p 5432 -U seed -W changeIM -d seedchange"
alias sshSEEDAWS="ssh -i seedchange.pem ec2-user@ec2-54-193-55-47.us-west-1.compute.amazonaws.com"
# ========================================================= #
# MacOSX-specific                                           #
# ========================================================= #
alias displayType='ioreg -lw0 | grep "EDID" | sed "/[^<]*</s///" | xxd -p -r | strings -6'

# ========================================================= #
# initialization scripts which are auto-generated           #
# ========================================================= #
eval "$(fasd --init auto)"

# some aws stuff: command to start the init script:
# alias nodejs='sh /etc/rc.d/init.d/nodejs restart'
alias gpob='gpo bugfix'
alias k9='killall -9 '
alias r='grunt'
alias grebase='git pull --rebase'
alias grup='grebase upstream'
alias grud='grup dev'

# added by travis gem
[ -f /Users/stites/.travis/travis.sh ] && source /Users/stites/.travis/travis.sh
alias vssh='vagrant ssh'
alias vhalt='vagrant halt'

### Added by the Heroku Toolbelt
export PATH="/usr/local/heroku/bin:$PATH"
alias treeClient='tree -I ./client/bower_components -I node_modules'
alias boom='gulp && gulp server'
alias sp='source ~/vagrant/stats/bin/activate'
alias pb='gulp browserify'
export NODE_PATH=/usr/local/lib/node:/usr/local/lib/node_modules
alias stats='source /Users/stites/vagrant/stats/bin/activate'

#######NWDP:
nwBase=/Users/stites/git/DemandCube/Sparkngin-developer-setup
alias nw="${nwBase}/setup.sh"
alias DP="${nwBase}/destroyAndCleanup.sh"


##########
# bash functions

function j {
  javac $1.java
  java $1 ${@:2}
}
export DOCKER_HOST=tcp://localhost:4243
