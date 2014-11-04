[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*

if [ -f ~/.bashrc ]; then
   source ~/.bashrc
fi
echo $PATH | grep -o -E "(^|:)/usr/local/heroku/bin:"

if [[ -z `echo $PATH | grep -o -e "(^|:)/bin"`]] ; then export PATH=$PATH:/bin fi
if [[ -z `echo $PATH | grep -o -e "(^|:)/sbin"`]] ; then export PATH=$PATH:/sbin fi
if [[ -z `echo $PATH | grep -o -e "(^|:)/usr/bin"`]] ; then export PATH=$PATH:/usr/bin fi
if [[ -z `echo $PATH | grep -o -e "(^|:)/usr/sbin"`]] ; then export PATH=$PATH:/usr/sbin fi
if [[ -z `echo $PATH | grep -o -e "(^|:)/usr/local/bin"`]] ; then export PATH=$PATH:/usr/local/bin fi
if [[ -z `echo $PATH | grep -o -e "(^|:)/usr/local/sbin"`]] ; then export PATH=$PATH:/usr/local/sbin fi

export PATH=$PATH:/usr/local/share/npm/bin
export PATH=$PATH:/Users/stites/.rvm/gems/ruby-1.9.3-p448/bin
export PATH=$PATH:/Users/stites/.rvm/gems/ruby-1.9.3-p448@global/bin
export PATH=$PATH:/Users/stites/.rvm/rubies/ruby-1.9.3-p448/bin
export PATH=$PATH:/Users/stites/.rvm/bin

[ -s $HOME/.nvm/nvm.sh ] && . $HOME/.nvm/nvm.sh # This loads NVM
