[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*

if [ -f ~/.bashrc ]; then
   source ~/.bashrc
fi
export PATH=$PATH:/usr/local/bin:/usr/local/sbin:/usr/local/share/npm/bin:/Users/stites/.rvm/gems/ruby-1.9.3-p448/bin:/Users/stites/.rvm/gems/ruby-1.9.3-p448@global/bin:/Users/stites/.rvm/rubies/ruby-1.9.3-p448/bin:/Users/stites/.rvm/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/Users/stites/.rvm/bin

[ -s $HOME/.nvm/nvm.sh ] && . $HOME/.nvm/nvm.sh # This loads NVM
export PATH=/usr/local/bin:$PATH


