{ pkgs }:

{
  knownPlugins = pkgs.vimPlugins;
  pluginDictionaries = [
    # everything global
    { names = [
        "LanguageClient-neovim"
#        "vim-plug"
        "supertab"
        "tslime-vim"
        "vimproc-vim"
        "vim-polyglot"
        "neomake"
        # "vim-bbye"
        "vim-indent-guides"
        "ultisnips"
#       "deoplete-nvim"
        # Plug 'nacitar/terminalkeys.vim'
        # Plug 'xolox/vim-session' | Plug 'xolox/vim-misc'
        # Plug 'qpkorr/vim-bufkill'
        "vim-eunuch"
        "undotree"

        # TMUX
        "vim-tmux-navigator"

        # GIT
        "rhubarb"   # Plug 'tpope/vim-rhubarb' | Plug 'tpope/vim-fugitive'
        "vim-gitgutter"

        # Bars, panels, and files
        "vim-airline"
        "vim-airline-themes"
        "fzf-vim" "fzfWrapper" # Plug 'junegunn/fzf.vim' | Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
        "ack-vim"
        "vim-unimpaired"
        "tagbar"

        # Text manipulation
        "align"                      # Plug 'vim-scripts/Align'
        # Plug 'simnalamburt/vim-mundo'
        "vim-commentary"                      # Plug 'tpope/vim-commentary'
        "tabular"                      # Plug 'godlygeek/tabular'
        "vim-indent-object"                      # Plug 'michaeljsmith/vim-indent-object'
        "vim-easymotion"                      # Plug 'easymotion/vim-easymotion'
        # Plug 'ConradIrwin/vim-bracketed-paste'
        "vim-multiple-cursors"                      # Plug 'terryma/vim-multiple-cursors'
        "vim-surround"                      # Plug 'tpope/vim-surround'                " http://vimawesome.com/plugin/surround-vim
        "vim-repeat"                      # Plug 'tpope/vim-repeat'                  " to be used with vim-surround
        "vim-speeddating"                      # Plug 'tpope/vim-speeddating'
        "syntastic"                      # Plug 'vim-syntastic/syntastic'
        "goyo"                      # Plug 'junegunn/goyo.vim'
        "rainbow_parentheses"                      # Plug 'junegunn/rainbow_parentheses.vim'
        # Plug 'pelodelfuego/vim-swoop'

        # Colorscheme
        "wombat256-vim"                         # Plug 'vim-scripts/wombat256.vim'
        # Plug 'endel/vim-github-colorscheme'
        # Plug 'mhartington/oceanic-next'
        # Plug 'freeo/vim-kalisi'

        # Productivity
        "vim-orgmode"                   # Plug 'jceb/vim-orgmode' | Plug 'mattn/calendar-vim' | Plug 'inkarkat/vim-SyntaxRange' | Plug 'vim-scripts/utl.vim'

        # Vim improvements:
        # Plug 'kana/vim-textobj-user' | Plug 'reedes/vim-textobj-sentence'

      ];
    }

    # THIS IS THE STOPPING POINT
    ];
#
#    { name = "dhall-vim";      ft_regex = "^dhall\$"; tag="lazy"; }
#    { name = "vim-closetag";   ft_regex = "^html\$";  tag="lazy"; } # also for xml
#    { name = "elm-vim";        ft_regex = "^elm\$";   tag="lazy"; }
#    { name = "vimtex";         ft_regex = "^latex\$"; tag="lazy"; }
#    { name = "clang_complete"; ft_regex = "^c\$";     tag="lazy"; } # also c++, cu, h, hpp
#
#    { names = [
#        "haskell-vim"
#        "hlint-refactor-vim"
#        "neco-ghc"
#        # ndmitchell/ghcid, rtp: plugins/nvim
#        # hspec/hspec.vim
#      ];
#      ft_regex = "^haskell\$";
#      tag="lazy";
#    }
#
#    # Python
#    # " Plug 'nvie/vim-flake8', { 'for': 'python' }
#    # " Plug 'vim-scripts/indentpython.vim', { 'for': 'python' }
#    # " Plug 'szymonmaszke/vimpyter'
#  ];
}
