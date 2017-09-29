" using vim-plug
call plug#begin('~/.config/nvim/bundle')

" Support bundles
Plug 'jgdavey/tslime.vim'
Plug 'Shougo/vimproc.vim', { 'do': 'make' }

" Plug 'ervandew/supertab'
Plug 'benekastah/neomake'
Plug 'moll/vim-bbye'
Plug 'nathanaelkane/vim-indent-guides'
Plug 'vim-scripts/gitignore'
Plug 'SirVer/ultisnips'
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'nacitar/terminalkeys.vim'
Plug 'xolox/vim-session' | Plug 'xolox/vim-misc'

" Git
" Plug 'tpope/vim-fugitive'
Plug 'int3/vim-extradite'

" Bars, panels, and files
Plug 'scrooloose/nerdtree'
Plug 'bling/vim-airline'
" Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'majutsushi/tagbar'                 " http://vimawesome.com/plugin/tagbar
Plug 'Xuyuanp/nerdtree-git-plugin'

" Text manipulation
Plug 'vim-scripts/Align'
Plug 'simnalamburt/vim-mundo'
Plug 'tpope/vim-commentary'
Plug 'godlygeek/tabular'
Plug 'michaeljsmith/vim-indent-object'
Plug 'easymotion/vim-easymotion'
Plug 'ConradIrwin/vim-bracketed-paste'
Plug 'terryma/vim-multiple-cursors'
Plug 'tpope/vim-surround'                " http://vimawesome.com/plugin/surround-vim
Plug 'vim-syntastic/syntastic'

" Allow pane movement to jump out of vim into tmux
Plug 'christoomey/vim-tmux-navigator'

" Haskell
Plug 'neovimhaskell/haskell-vim',     { 'for': 'haskell' }
Plug 'eagletmt/ghcmod-vim',           { 'for': 'haskell' }
Plug 'eagletmt/neco-ghc',             { 'for': 'haskell' }
Plug 'Twinside/vim-hoogle',           { 'for': 'haskell' }
Plug 'mpickering/hlint-refactor-vim', { 'for': 'haskell' }


Plug 'ElmCast/elm-vim', { 'for': 'elm' }

" Haskell, always-on
Plug 'ndmitchell/ghcid', { 'rtp': 'plugins/nvim' }
Plug 'hspec/hspec.vim'

" Colorscheme
Plug 'vim-scripts/wombat256.vim'
Plug 'endel/vim-github-colorscheme'
Plug 'mhartington/oceanic-next'
Plug 'freeo/vim-kalisi'

" LaTeX plugins
Plug 'lervag/vimtex', { 'for': 'latex' }

" Productivity
" Plug 'wakatime/vim-wakatime'

" Python
" Plug 'nvie/vim-flake8', { 'for': 'python' }
" Plug 'vim-scripts/indentpython.vim', { 'for': 'python' }


" Automatically executes `filetype plugin indent on` and `syntax enable`.
" You can revert the settings after the call. (e.g. filetype indent off, syntax off, etc.)
call plug#end()
