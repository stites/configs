{ vimUtils, fetchFromGitHub, stdenv }:

{
  vim-textobj-sentence = vimUtils.buildVimPlugin rec {
    # last update 2017
    name = "vim-textobj-sentence";
    src = builtins.fetchTarball { url = "https://github.com/reedes/${name}/archive/master.tar.gz"; };
    meta = {
      homepage = https://github.com/reedes/vim-textobj-sentence;
      maintainers = [ ];
    };
  };
   vim-textobj-user = vimUtils.buildVimPlugin rec {
    # last update 2018
    name = "vim-textobj-user";
    src = builtins.fetchTarball { url = "https://github.com/kana/${name}/archive/master.tar.gz"; };
    meta = {
      homepage = https://github.com/kana/vim-textobj-user;
      maintainers = [ ];
    };
  };

  papercolor-theme = vimUtils.buildVimPlugin rec {
    # last update 2019
    name = "papercolor-theme";
    src = builtins.fetchTarball { url = "https://github.com/NLKNguyen/${name}/archive/master.tar.gz"; };
    meta = {
      homepage = https://github.com/NLKNguyen/papercolor-theme;
      maintainers = [ stdenv.lib.maintainers.stites ];
    };
  };

  vim-bufkill = vimUtils.buildVimPlugin rec {
    # last update 2018
    name = "vim-bufkill";
    src = builtins.fetchTarball { url = "https://github.com/qpkorr/${name}/archive/master.tar.gz"; };
    meta = {
      homepage = https://github.com/qpkorr/vim-bufkill;
      maintainers = [ ];
    };
  };

  vim-bbye = vimUtils.buildVimPlugin rec {
    # last update 2018
    name = "vim-bbye";
    src = builtins.fetchTarball { url = "https://github.com/moll/${name}/archive/master.tar.gz"; };
    meta = {
      homepage = https://github.com/moll/vim-bbye;
      maintainers = [ ];
    };
  };

  closetag-vim = vimUtils.buildVimPlugin rec {
    # last update 2015
    name = "closetag-vim";
    src = builtins.fetchTarball { url = "https://github.com/docunext/closetag.vim/archive/master.tar.gz"; };
    meta = {
      homepage = https://github.com/docunext/closetag.vim;
      maintainers = [ stdenv.lib.maintainers.stites ];
    };
  };

  terminalkeys-vim = vimUtils.buildVimPlugin {
    # last update 2012
    name = "terminalkeys-vim";
    src = builtins.fetchTarball { url = "https://github.com/nacitar/terminalkeys.vim/archive/master.tar.gz"; };
    meta = {
      homepage = https://github.com/nacitar/terminalkeys.vim;
      maintainers = [ ];
    };
  };

  oceanic-next = vimUtils.buildVimPlugin rec {
    # last update 2018
    name = "oceanic-next";
    src = builtins.fetchTarball { url = "https://github.com/mhartington/${name}/archive/master.tar.gz"; };
    meta = {
      homepage = https://github.com/mhartington/oceanic-next;
      maintainers = [ ];
    };
  };

  yaflandia = vimUtils.buildVimPlugin rec {
    # last update 2017
    name = "yaflandia";
    src = builtins.fetchTarball { url = "https://github.com/JBakamovic/${name}/archive/master.tar.gz"; };
    meta = {
      homepage = https://github.com/JBakamovic/yaflandia;
      maintainers = [ ];
    };
  };

  vim-misc = vimUtils.buildVimPlugin rec {
    # last update 2015
    name = "vim-misc";
    src = builtins.fetchTarball { url = "https://github.com/xolox/${name}/archive/master.tar.gz"; };
    meta = {
      homepage = https://github.com/xolox/vim-misc;
      maintainers = [ ];
    };
  };

  vim-session = vimUtils.buildVimPlugin rec {
    # last update 2015
    name = "vim-session";
    src = builtins.fetchTarball { url = "https://github.com/xolox/${name}/archive/master.tar.gz"; };
    meta = {
      homepage = https://github.com/xolox/vim-session;
      maintainers = [ ];
    };
  };

  hspec-vim = vimUtils.buildVimPlugin {
    # last update 2019
    name = "hspec-vim";
    src = builtins.fetchTarball { url = "https://github.com/hspec/hspec.vim/archive/master.tar.gz"; };
    meta = {
      homepage = https://github.com/hspec/hspec.vim;
      maintainers = [ ];
    };
  };

  vim-endwise = vimUtils.buildVimPlugin rec {
    # last update 2018
    name = "vim-endwise";
    src = builtins.fetchTarball { url = "https://github.com/tpope/${name}/archive/master.tar.gz"; };
    meta = {
      homepage = https://github.com/tpope/vim-endwise;
      maintainers = [ ];
    };
  };

  hledger-vim = vimUtils.buildVimPlugin rec {
    # last update 2019
    name = "hledger-vim";
    src = builtins.fetchTarball { url = "https://github.com/anekos/${name}/archive/master.tar.gz"; };
    meta = {
      homepage = https://github.com/anekos/hledger-vim;
      maintainers = [ ];
    };
  };

  vim-swoop = vimUtils.buildVimPlugin rec {
    # last update 2017
    name = "vim-swoop";
    src = builtins.fetchTarball { url = "https://github.com/pelodelfuego/${name}/archive/master.tar.gz"; };
    meta = {
      homepage = https://github.com/pelodelfuego/vim-swoop;
      maintainers = [ ];
    };
  };

  vim-bracketed-paste = vimUtils.buildVimPlugin rec {
    # last update 2018
    name = "vim-bracketed-paste";
    src = builtins.fetchTarball { url = "https://github.com/ConradIrwin/vim-bracketed-paste/archive/master.tar.gz"; };
    meta = {
      homepage = https://github.com/ConradIrwin/vim-bracketed-paste;
      maintainers = [ ];
    };
  };

  vim-mundo = vimUtils.buildVimPlugin rec {
    # last update 2019
    name = "vim-mundo";
    src = builtins.fetchTarball { url = "https://github.com/simnalamburt/${name}/archive/3.1.0-1.tar.gz"; };
    meta = {
      homepage = https://github.com/simnalamburt/vim-mundo;
      maintainers = [ ];
    };
  };

  chromatica-nvim = vimUtils.buildVimPlugin rec {
    # last update 2019
    name = "chromatica-nvim";
    src = builtins.fetchTarball { url = "https://github.com/arakashic/chromatica.nvim/archive/master.tar.gz"; };
    meta = {
      homepage = https://github.com/arakashic/chromatica.nvim;
      maintainers = [ ];
      shortDescription = "Clang based syntax highlighting for Neovim";
    };
  };

  lessspace-vim = vimUtils.buildVimPlugin rec {
    # last update 2019
    name = "lessspace-vim";
    src = builtins.fetchTarball { url = "https://github.com/thirtythreeforty/lessspace.vim/archive/master.tar.gz"; };
    meta = {
      homepage = https://github.com/thirtythreeforty/lessspace.vim;
      maintainers = [ ];
      shortDescription = "Strip trailing whitespace from files, but only on the lines you edit or visit in Insert mode";
    };
  };

  vim-highlightedyank = vimUtils.buildVimPlugin rec {
    # last update 2018
    name = "vim-highlightedyank";
    src = builtins.fetchTarball { url = "https://github.com/machakann/${name}/archive/master.tar.gz"; };
    meta = {
      homepage = https://github.com/machakann/vim-highlightedyank;
      maintainers = [ ];
    };
  };

  vim-sandwich = vimUtils.buildVimPlugin rec {
    # last update 2019
    name = "vim-sandwich";
    src = builtins.fetchTarball { url = "https://github.com/machakann/${name}/archive/master.tar.gz"; };

    meta = {
      homepage = https://github.com/machakann/vim-sandwich;
      maintainers = [ ];
    };
  };

  ghcid = vimUtils.buildVimPlugin rec {
    # last update 2019
    name = "ghcid";
    src = (builtins.fetchTarball { url = "https://github.com/ndmitchell/${name}/archive/v0.7.4.tar.gz"; }) + "/plugins/nvim";

    meta = {
      homepage = https://github.com/ndmitchell/ghcid;
      maintainers = [ ];
    };
  };

  vim-kalisi = vimUtils.buildVimPlugin rec {
    # last update 2016
    name = "vim-kalisi";
    src = builtins.fetchTarball { url = "https://github.com/freeo/${name}/archive/master.tar.gz"; };
    meta = {
      homepage = https://github.com/freeo/vim-kalisi;
      maintainers = [ ];
    };
  };

  utl-vim = vimUtils.buildVimPlugin {
    # last update 2008
    name = "utl-vim";
    src = builtins.fetchTarball { url = "https://github.com/vim-scripts/utl.vim/archive/master.tar.gz"; };
    meta = {
      homepage = https://github.com/vim-scripts/utl.vim;
      maintainers = [ ];
    };
  };

  vim-github-colorscheme = vimUtils.buildVimPlugin rec {
    # last update 2014
    name = "vim-github-colorscheme";
    src = builtins.fetchTarball { url = "https://github.com/endel/vim-github-colorscheme/archive/master.tar.gz"; };
    meta = {
      homepage = https://github.com/endel/vim-github-colorscheme;
      maintainers = [ ];
    };
  };

  tabnine-vim = vimUtils.buildVimPlugin rec {
    name = "tabnine-vim";
    src = builtins.fetchTarball { url = "https://github.com/zxqfl/tabnine-vim/archive/master.tar.gz"; };
    meta = {
      homepage = https://github.com/zxqfl/tabnine-vim;
      maintainers = [ ];
    };
  };

  vim-brittany = vimUtils.buildVimPlugin rec {
    # last update 2019
    name = "vim-brittany";
    src = builtins.fetchTarball { url = "https://github.com/meck/vim-brittany/archive/master.tar.gz"; };
    meta = {
      homepage = https://github.com/meck/vim-brittany;
      maintainers = [ stdenv.lib.maintainers.stites ];
    };
  };
}
