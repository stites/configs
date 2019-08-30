{ lib, pkgs, pluginBuilder, ... }:
{
  rc = lib.strings.concatStringsSep "\n" [

    ''
      let @x=':%s/<Plug>_*//g'
    ''
  ];
  plugins = with pkgs.vimPlugins; [
    # alignment stuff
    align                   # align characters
    vim-easy-align          # more comprehensive alignment???
    tabular                 # 'godlygeek/tabular'

    # Text manipulation
    vim-sensible            # make vim sane
    vim-unimpaired          # pairs of handy bracket mappings
    (pluginBuilder rec {
      name = "vim-mundo";
      tarball = "${homepage}/archive/3.1.0-1.tar.gz";
      homepage = https://github.com/simnalamburt/vim-mundo;
    })
    vim-commentary          # 'tpope/vim-commentary'
    vim-indent-object       # 'michaeljsmith/vim-indent-object'
    vim-easymotion          # 'easymotion/vim-easymotion'
    (pluginBuilder {
      # last update 2018
      name = "vim-bracketed-paste";
      tarball = "https://github.com/ConradIrwin/vim-bracketed-paste/archive/master.tar.gz";
      homepage = https://github.com/ConradIrwin/vim-bracketed-paste;
    })
    vim-repeat              # 'tpope/vim-repeat'     " to be used with vim-surround
    vim-speeddating         # 'tpope/vim-speeddating'
    syntastic               # 'vim-syntastic/syntastic'
    vim-github-dashboard    # 'junegunn/vim-github-dashboard'
    (pluginBuilder rec {
      name = "vim-swoop";
      tarball = "https://github.com/pelodelfuego/${name}/archive/master.tar.gz";
      homepage = https://github.com/pelodelfuego/vim-swoop;
    })
  ];
}
