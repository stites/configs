with import <nixpkgs> {};

vim_configurable.override {
  config.vim = {
    ruby = true;
  };
  ruby = ruby;
}

vim_configurable.customize {
    # Specifies the vim binary name.
    # E.g. set this to "my-vim" and you need to type "my-vim" to open this vim
    # This allows to have multiple vim packages installed (e.g. with a different set of plugins)
    name = "vim";
    vimrcConfig.customRC = ''
        # Here one can specify what usually goes into `~/.vimrc`
        syntax enable
    '';

    # Use the default plugin list shipped with nixpkgs
    vimrcConfig.vam.knownPlugins = pkgs.vimPlugins;

    # Managed by `vam` (a vim plugin manager): https://nixos.org/wiki/Vim_plugins
    vimrcConfig.vam.pluginDictionaries = [
      { # load always
        names = [
          "github:vim-scripts/align" # junegunn/vim-easy-align may be better
          "Tagbar"
          "github:shougo/vimproc.vim"
          "github:vim-scripts/wombat256.vim"
          "github:christoomey/vim-tmux-navigator"
          "github:gisphm/vim-gitignore"
          "vim-snippets"
          "commentary"
          "The_NERD_tree"
          "github:Xuyuanp/nerdtree-git-plugin'
          "github:jgdavey/tslime.vim"
          "Supertab"
          "fugitive"
          "extradite"
          "github:vim-airline/vim-airline-themes"
          "github:benekastah/neomake"
          "github:michaeljsmith/vim-indent-object"
          "github:nathanaelkane/vim-indent-guides"
          "github:simnalamburt/vim-mundo"
          "Tabular"
          "github:moll/vim-bbye"
          "vim-airline"
          "github:lokaltog/vim-easymotion"
          "ctrlp"
          "vim-multiple-cursors"
        ];
      };

      { name = "ghcmod"; filename_regex = "^.hs\$"; };
      { name = "github:eagletmt/neco-ghc"; filename_regex = "^.hs\$"; };
      { name = "github:mpickering/hlint-refactor-vim"; filename_regex = "^.hs\$"; };
      { name = "Hoogle"; filename_regex = "^.hs\$"; };
      { name = "github:neovimhaskell/haskell-vim"; filename_regex = "^.hs\$"; };
      { name = "github:enomsg/vim-haskellConcealPlus"; filename_regex = "^.hs\$"; };
      { name = "github:myfreeweb/intero.nvim"; filename_regex = "^.hs\$"; };

      { name = "github:LnL7/vim-nix"; filename_regex = "^.nix\$"; };

      # these could be cool / worth looking into

      # "github:elmcast/elm-vim"
      # "github:fatih/vim-go"
      # "github:idris-hackers/idris-vim"
      # "github:raichoo/purescript-vim"
      # "github:rust-lang/rust.vim"
      # "github:shougo/neocomplete.vim"
      # "github:shougo/neosnippet-snippets"
      # "github:shougo/neosnippet.vim"
      # "github:tomasr/molokai"
      # "rainbow_parentheses"
      # "sensible"
      # "vim-easy-align"
      # "ctrlp-py-matcher"
      # "ctrlp-z"

    ];
}
