To initiate configs on a computer run:

    . setup.sh {guest|host}

Bash colors from (systhread)[http://www.systhread.net/texts/200703bashish.php]

### NOTE:
things have gotten a little more complicated with my setup. everything is now modularized into .bashrc_<tool> files. the current oversight is that you will not nessecarily know which tools are installed - there needs to be a check for the standard tools and a script which gives the option of installing them.

For the time being, I'll just write a list:
+ nvm
+ emacs prelude
+ pyenv
+ fasd
+ ... and more!

things outside of this:
install nix-env closure
install brew for everything else (you may have to uninstall nss-cacerts from nix)
sync git repositories (usually located in ~/git)
sync haskell-related git repositories (usually located in ~/git/hs)
download fonts (clone powerline/fonts, run install.sh)
install haskell-vim-now
 - remember to include ~/.config/haskell-vim-now/scripts/neovim.sh somewhere if you are going to use neovim (which you should)
download iterm color schemes:
 - git clone mbadolato/iTerm2-Color-Schemes
 - 
