{ lib, pkgs, stdenv, fetchgit, vimUtils, ... }:
let
  customPlugins = {
    papercolor-theme = vimUtils.buildVimPlugin {
      name = "papercolor-theme-git-2018-09-03";
      src = fetchgit {
        url = "https://github.com/NLKNguyen/papercolor-theme.git";
        rev = "5bd7d5b";
        sha256 = "061551ih3pndnlimxqwa5hrxwn8knpzf87hr6q3zwr9pdfmqpm9l";
      };
      meta = {
        homepage = https://github.com/NLKNguyen/papercolor-theme;
        maintainers = [ stdenv.lib.maintainers.stites ];
      };
    };
    vim-bufkill = vimUtils.buildVimPlugin {
      name = "vim-bufkill";
      src = fetchgit {
        url = "https://github.com/qpkorr/vim-bufkill.git";
        rev = "795dd38";
        sha256 = "1nji86vjjbfjw4xy52yazq53hrlsr7v30xkx2awgiakz7ih0bdxa";
      };
      meta = {
        homepage = https://github.com/qpkorr/vim-bufkill;
        maintainers = [ ];
      };
    };


    vim-session = vimUtils.buildVimPlugin {
      name = "vim-session";
      src = fetchgit {
        url = "https://github.com/xolox/vim-session.git";
        rev = "9e9a608";
        sha256 = "0r6k3fh0qpg95a02hkks3z4lsjailkd5ddlnn83w7f51jj793v3b";
      };
      meta = {
        homepage = https://github.com/xolox/vim-session;
        maintainers = [ ];
      };
    };

    vim-bbye = vimUtils.buildVimPlugin {
      name = "vim-bbye";
      src = fetchgit {
        url = "https://github.com/moll/vim-bbye.git";
        rev = "25ef93a";
        sha256 = "0dlifpbd05fcgndpkgb31ww8p90pwdbizmgkkq00qkmvzm1ik4y4";
      };
      meta = {
        homepage = https://github.com/moll/vim-bbye;
        maintainers = [ ];
      };
    };

    closetag-vim = vimUtils.buildVimPlugin {
      name = "closetag-vim";
      src = fetchgit {
        url = "https://github.com/docunext/closetag.vim.git";
        rev = "a52525b";
        sha256 = "1khnvyxhg2x1rn11p46prpfm11fywyp9jni8dc7zrqib1lhrlsmj";
      };
      meta = {
        homepage = https://github.com/docunext/closetag.vim;
        maintainers = [ stdenv.lib.maintainers.stites ];
      };
    };
    terminalkeys-vim = vimUtils.buildVimPlugin {
      name = "terminalkeys-vim";
      src = fetchgit {
        url = "https://github.com/nacitar/terminalkeys.vim.git";
        rev = "f7c9125";
        sha256 = "130sn8d2gzrw628rychk53946vvpwhsj2layhpsa0srp0h9l6snx";
      };
      meta = {
        homepage = https://github.com/nacitar/terminalkeys.vim;
        maintainers = [ ];
      };
    };
    vim-brittany = vimUtils.buildVimPlugin {
      name = "vim-brittany-2018-07-22";
      src = fetchgit {
        url = "https://github.com/meck/vim-brittany.git";
        rev = "863e2af";
        sha256 = "1mrv1sc837v3zmq4wl09kkb3hil7add99npjnflivdm3fc8lar3q";
      };
      meta = {
        homepage = https://github.com/meck/vim-brittany;
        maintainers = [ stdenv.lib.maintainers.stites ];
      };
    };
  };
in
{
  xdg.dataFile = {
    "vim_gmake" = {
      executable = true;
      target = "../bin/vim_gmake";
      text = ''
        #!/usr/bin/env sh
        case "$(uname -o)" in
          "FreeBSD") gmake ;;
          *) make ;;
        esac
      '';
    };

    "vl" = {
      executable = true;
      target = "../bin/vl";
      source = ./vl.sh;
    };

    "vim-plug" = {
      target = "nvim/site/autoload/plug.vim";
      source = builtins.fetchurl {
        url = "https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim";
        sha256 = "1rpqfgxrws6298yhaj395czmqa7nlicg5s900vnr4gf84npmr2p6";
      };
    };
  };

  config = {
    enable = true;
    extraPython3Packages = (ps: with ps; [
      mccabe
      mypy
      nose
      pycodestyle
      pydocstyle

      jedi
      flake8
      pygments
      pytest-mypy
      python-language-server
      pyls-isort
      pyls-mypy
      pyflakes
      yapf
    ]);
    viAlias = true;
    vimAlias = true;

    # CHECK OUT THIS FOR UPDATED CONTENT: https://nixos.wiki/wiki/Vim
    configure = {
      # start = [ pkgs.vimPlugins.LanguageClient-neovim ];
      # customRC = (import ./customRC.nix { inherit lib; }).customRC;
      customRC = lib.strings.concatStringsSep "\n" [
        ''
        tnoremap <Esc> <C-\><C-n>
        set encoding=utf-8
        ''
        ''
        let g:gitgutter_git_executable="${pkgs.git}/bin/git"
        ''

        (builtins.readFile ./defaults.vim     )
        (builtins.readFile ./haskell.vim      )
        (builtins.readFile ./elm.vim          )
        (builtins.readFile ./plugins-config.vim)
        (builtins.readFile ./ui.vim           )
        (builtins.readFile ./spelling.vim     )
        (builtins.readFile ./python.vim       )

        ''
        let g:vimtex_compiler_progname = 'nvr'
        let @x=':%s/<Plug>_*//g'
        ''

        ''
        " Files, backups and undo {{{

        " Turn backup off, since most stuff is in Git anyway...
        set nobackup
        set nowb
        set noswapfile
        ''

        ''
        fun! <SID>StripTrailingWhitespaces()
            let l = line(".")
            let c = col(".")
            %s/\s\+$//e
            call cursor(l, c)
        endfun

        autocmd FileType haskell,c,cpp,java,php,ruby,python autocmd BufWritePre <buffer> :call <SID>StripTrailingWhitespaces()
        ''

        # No longer nessecary in nix!
        # ''
        # " Source the vimrc file after saving it
        # augroup sourcing
        #   autocmd!
        #   if has('nvim')
        #     autocmd bufwritepost init.vim source $MYVIMRC
        #   else
        #     autocmd bufwritepost .vimrc source $MYVIMRC
        #   endif
        # augroup END
        # ''

        ''
        " Open file prompt with current path
        nmap <leader>e :e <C-R>=expand("%:p:h") . '/'<CR>

        " Show undo tree
        nmap <silent> <leader>u :MundoToggle<CR>

        " Fuzzy find files
        nnoremap <silent> <Leader><space> :CtrlP<CR>
        let g:ctrlp_max_files=0
        let g:ctrlp_show_hidden=1
        let g:ctrlp_custom_ignore = { 'dir': '\v[\/](.git|.cabal-sandbox|.stack-work)$' }

        " Allow saving of files as sudo when I forgot to start vim using sudo.
        cmap w!! w !sudo tee > /dev/null %

        " }}}

        " Text, tab and indent related {{{

        " Use spaces instead of tabs
        set expandtab

        " Be smart when using tabs ;)
        set smarttab

        " 1 tab == 2 spaces
        set shiftwidth=2
        set tabstop=2

        " Linebreak on 500 characters
        set lbr
        set tw=500

        set ai "Auto indent
        set si "Smart indent
        set wrap "Wrap lines

        " Copy and paste to os clipboard
        nmap <leader>y "+y
        vmap <leader>y "+y
        nmap <leader>d "+d
        vmap <leader>d "+d
        nmap <leader>p "+p
        vmap <leader>p "+p

        " }}}
        ''

        ''
        " Visual mode related {{{

        " Visual mode pressing * or # searches for the current selection
        " Super useful! From an idea by Michael Naumann
        vnoremap <silent> * :call VisualSelection('f', '''''')<CR>
        vnoremap <silent> # :call VisualSelection('b', '''''')<CR>

        " }}}

        " Moving around, tabs, windows and buffers {{{

        " Treat long lines as break lines (useful when moving around in them)
        nnoremap j gj
        nnoremap k gk

        noremap <c-h> <c-w>h
        noremap <c-k> <c-w>k
        noremap <c-j> <c-w>j
        noremap <c-l> <c-w>l

        " Disable highlight when <leader><cr> is pressed
        " but preserve cursor coloring
        nmap <silent> <leader><cr> :noh\|hi Cursor guibg=red<cr>

        " Return to last edit position when opening files (You want this!)
        augroup last_edit
          autocmd!
          autocmd BufReadPost *
               \ if line("'\"") > 0 && line("'\"") <= line("$") |
               \   exe "normal! g`\"" |
               \ endif
        augroup END
        " Remember info about open buffers on close
        set viminfo^=%

        " Open window splits in various places
        nmap <leader>sh :leftabove  vnew<CR>
        nmap <leader>sl :rightbelow vnew<CR>
        nmap <leader>sk :leftabove  new<CR>
        nmap <leader>sj :rightbelow new<CR>

        " Manually create key mappings (to avoid rebinding C-\)
        let g:tmux_navigator_no_mappings = 1

        nnoremap <silent> <C-h> :TmuxNavigateLeft<cr>
        nnoremap <silent> <C-j> :TmuxNavigateDown<cr>
        nnoremap <silent> <C-k> :TmuxNavigateUp<cr>
        nnoremap <silent> <C-l> :TmuxNavigateRight<cr>

        " don't close buffers when you aren't displaying them
        set hidden

        " previous buffer, next buffer
        nnoremap <leader>bp :bp<cr>
        nnoremap <leader>bn :bn<cr>
        
        " close every window in current tabview but the current
        nnoremap <leader>bo <c-w>o
        
        " delete buffer without closing pane
        noremap <leader>bd :Bd<cr>
        
        " fuzzy find buffers
        noremap <leader>b<space> :CtrlPBuffer<cr>
        
        set incsearch
        set nohlsearch

        " Neovim terminal configurations
        if has('nvim')
          " Use <Esc> to escape terminal insert mode
          tnoremap <Esc> <C-\><C-n>
          " Make terminal split moving behave like normal neovim
          tnoremap <c-h> <C-\><C-n><C-w>h
          tnoremap <c-j> <C-\><C-n><C-w>j
          tnoremap <c-k> <C-\><C-n><C-w>k
          tnoremap <c-l> <C-\><C-n><C-w>l
        endif
        " }}}
        ''

        ''
        " Status line {{{

        " Always show the status line
        set laststatus=2

        " }}}
        ''

        ''
        " Editing mappings {{{

        " Utility function to delete trailing white space
        func! DeleteTrailingWS()
          exe "normal mz"
          %s/\s\+$//ge
          exe "normal `z"
        endfunc

        " }}}

        " Spell checking {{{

        " Pressing ,ss will toggle and untoggle spell checking
        map <leader>ss :setlocal spell!<cr>

        " }}}

        " Helper functions {{{

        function! CmdLine(str)
          exe "menu Foo.Bar :" . a:str
          emenu Foo.Bar
          unmenu Foo
        endfunction

        function! VisualSelection(direction, extra_filter) range
          let l:saved_reg = @"
          execute "normal! vgvy"

          let l:pattern = escape(@", '\\/.*$^~[]')
          let l:pattern = substitute(l:pattern, "\n$", "", "")

          if a:direction == 'b'
            execute "normal ?" . l:pattern . "^M"
          elseif a:direction == 'gv'
            call CmdLine("vimgrep " . '/'. l:pattern . '/' . ' **/*.' . a:extra_filter)
          elseif a:direction == 'replace'
            call CmdLine("%s" . '/'. l:pattern . '/')
          elseif a:direction == 'f'
            execute "normal /" . l:pattern . "^M"
          endif

          let @/ = l:pattern
          let @" = l:saved_reg
        endfunction

        " }}}

        " Slime {{{

        vmap <silent> <Leader>rs <Plug>SendSelectionToTmux
        nmap <silent> <Leader>rs <Plug>NormalModeSendToTmux
        nmap <silent> <Leader>rv <Plug>SetTmuxVars

        " }}}

        " Alignment {{{

        " Stop Align plugin from forcing its mappings on us
        let g:loaded_AlignMapsPlugin=1
        " Align on equal signs
        map <Leader>a= :Align =<CR>
        " Align on commas
        map <Leader>a, :Align ,<CR>
        " Align on pipes
        map <Leader>a<bar> :Align <bar><CR>
        " Prompt for align character
        map <leader>ap :Align
        " }}}

        " Tags {{{

        map <leader>tt :TagbarToggle<CR>

        set tags+=./tags;$HOME,./codex.tags;$HOME
        set cst
        set csverb

        " IGNORES ARE HERE BECAUSE THEY INTERFERE WITH CTAG LOOKUP
        " Web ignores
        set wildignore+=*.min.*
        " Haskell ignores
        set wildignore+=*.stack-work*
        " C ignores
        set wildignore+=*.so
        " Python ignores
        set wildignore+=__pycache__/*,*.py[cod],*$py.class,*.ipynb,.Python,env/*,build/*
        set wildignore+=develop-eggs/*,dist/*,downloads/*,eggs/*,.eggs/*,lib/*,lib64/*
        set wildignore+=parts/*,sdist/*,var/*,*.egg-info/*,.installed.cfg,*.egg,*.manifest
        set wildignore+=*.spec,pip-log.txt,pip-delete-this-directory.txt,htmlcov/*
        set wildignore+=__pycache__/*,.tox/*,.coverage,.coverage.*,.cache,nosetests.xml
        set wildignore+=coverage.xml,cover,.hypothesis/*,*.mo,*.pot,*.log,local_settings.py
        set wildignore+=instance/*,.webassets-cache,.scrapy,docs/_build/*,target/*
        set wildignore+=.ipynb_checkpoints,.python-version,celerybeat-schedule,.env,venv/*
        set wildignore+=ENV/*,.spyderproject,.ropeproject,.DS_Store,*.sublime-workspace

        " case-insensitive search
        set wildignorecase
        " }}}
        ''

        ''
        " Git {{{

        let g:extradite_width = 60
        " Hide messy Ggrep output and copen automatically
        function! NonintrusiveGitGrep(term)
          execute "copen"
          " Map 't' to open selected item in new tab
          execute "nnoremap <silent> <buffer> t <C-W><CR><C-W>T"
          execute "silent! Ggrep " . a:term
          execute "redraw!"
        endfunction

        command! -nargs=1 GGrep call NonintrusiveGitGrep(<q-args>)
        nmap <leader>gs :Gstatus<CR>
        nmap <leader>gg :copen<CR>:GGrep 
        nmap <leader>gl :Extradite!<CR>
        nmap <leader>gd :Gdiff<CR>
        nmap <leader>gb :Gblame<CR>

        function! CommittedFiles()
          " Clear quickfix list
          let qf_list = []
          " Find files committed in HEAD
          let git_output = system("git diff-tree --no-commit-id --name-only -r HEAD\n")
          for committed_file in split(git_output, "\n")
            let qf_item = {'filename': committed_file}
            call add(qf_list, qf_item)
          endfor
          " Fill quickfix list with them
          call setqflist(qf_list)
        endfunction

        " Show list of last-committed files
        nnoremap <silent> <leader>g? :call CommittedFiles()<CR>:copen<CR>

        " }}}

        " Completion {{{
        set completeopt+=longest

        " Use buffer words as default tab completion
        let g:SuperTabDefaultCompletionType = '<c-x><c-p>'

        " }}}
        ''

        ''
        let g:LanguageClient_serverCommands = {
            \ 'haskell': ['hie-wrapper', '--lsp'],
            \ 'haskell.hspec': ['hie-wrapper', '--lsp'],
            \ 'rust': ['rustup', 'run', 'stable', 'rls'],
            \ 'javascript': ['javascript-typescript-stdio'],
            \ 'javascript.jsx': ['tcp://127.0.0.1:2089'],
            \ 'python': ['pyls'],
            \ }
        ''

      ];

      # store your plugins in Vim packages
      vam.knownPlugins = pkgs.vimPlugins // customPlugins;
      vam.pluginDictionaries = [
        # load always
        { names = [
          "vim-sensible"

          "LanguageClient-neovim"
          # Plug 'autozimu/LanguageClient-neovim', {
          #     \ 'branch': 'next',
          #     \ 'do': 'bash ./install.sh',
          #     \ }

          # Use with LSP to search for, substitute, and abbreviate multiple variants of a word
          "vim-abolish"

          # Colorschemes, icons, and themes
          "vim-airline"
          "vim-airline-themes"
          "vim-devicons"
          "gruvbox"
          "papercolor-theme"

          # 'vim-scripts/wombat256.vim'
          # 'endel/vim-github-colorscheme'
          # 'mhartington/oceanic-next'
          # 'freeo/vim-kalisi'
          # 'JBakamovic/yaflandia'

          # ??????????
          # "webapi-vim"

          # git
          "vim-fugitive"
          "vim-gitgutter"
          "vim-rhubarb" # depends on fugitive

          # support
          "tslime"
          "vimproc"       # Plug 'Shougo/vimproc.vim', { 'do': 'vim_gmake' }
          "vim-polyglot"

          # "supertab"
          "neomake"
          "vim-bbye"
          "vim-indent-guides"
          # "gitignore"
          "ultisnips"
          "deoplete-nvim"               # Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
          "terminalkeys-vim"           # Plug 'nacitar/terminalkeys.vim'
          # "vim-misc" "vim-session" # Plug 'xolox/vim-session' | Plug 'xolox/vim-misc'
          "vim-bufkill"                # Plug 'qpkorr/vim-bufkill'
          "vim-eunuch"                 # Plug 'tpope/vim-eunuch'
          "undotree"               # Plug 'mbbill/undotree'

          # syntax highlighting
          "rust-vim"
          "vim-nix"

          # Bars, panels, and files
          "fzf-vim"
          # Plug 'junegunn/fzf.vim' | Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
          "ack-vim"
          "vim-unimpaired"
          # "tagbar"           # " Plug 'majutsushi/tagbar' " http://vimawesome.com/plugin/tagbar
          # Plug 'mileszs/ack.vim'
          # Plug 'tpope/vim-unimpaired'

          # Text manipulation
          "align"      # 'vim-scripts/Align'
          # ADD "vim-mundo" # 'simnalamburt/vim-mundo'
          "vim-commentary"   # 'tpope/vim-commentary'
          "tabular"      # 'godlygeek/tabular'
          "vim-indent-object"    # 'michaeljsmith/vim-indent-object'
          "vim-easymotion"       # 'easymotion/vim-easymotion'
          # ADD "vim-bracketet-paste"  # 'ConradIrwin/vim-bracketed-paste'
          "vim-multiple-cursors" # 'terryma/vim-multiple-cursors'
          "vim-surround"     # 'tpope/vim-surround'                " http://vimawesome.com/plugin/surround-vim
          "vim-repeat"       # 'tpope/vim-repeat'                  " to be used with vim-surround
          "vim-speeddating"  # 'tpope/vim-speeddating'
          "syntastic"# 'vim-syntastic/syntastic'
          "goyo-vim"      # 'junegunn/goyo.vim'
          "rainbow_parentheses-vim" # 'junegunn/rainbow_parentheses.vim'
          # ADD "vim-swoop" # 'pelodelfuego/vim-swoop'

          # Allow pane movement to jump out of vim into tmux
          "vim-tmux-navigator"

          # Vim improvements:
          # ADD  Plug 'kana/vim-textobj-user' | Plug 'reedes/vim-textobj-sentence'

        ]; }

        # # C/C++ development
        # Plug 'JBakamovic/cxxd-vim', { 'for': ['c', 'cpp'] }
        # " C++ plugins
        # Plug 'Rip-Rip/clang_complete', { 'for': 'c' }

        # Dhall
        { ft_regex = "^dhall\$"; 
          name = "dhall-vim";
        }

        # Haskell
        { ft_regex = "^haskell\$"; 
          names = [
            "haskell-vim"        # "neovimhaskell/haskell-vim"     
            "vim-hoogle"         # "Twinside/vim-hoogle"           
            "hlint-refactor-vim" # "mpickering/hlint-refactor-vim" 
            "neco-ghc"           # "eagletmt/neco-ghc"             
            "vim-brittany"       # "meck/vim-brittany"             
          ];
        }

        # HTML, XML
        { ft_regex = "^(h|x)tml\$"; 
          names = [
            "closetag-vim" # "docunext/closetag.vim"     
          ];
        }

        # LaTex
        { ft_regex = "^latex\$"; 
          names = [
            "vimtex"      # "lervag/vimtex"     
          ];
        }

        # # HLedger
        # { ft_regex = "^journal\$"; 
        #   names = [
        #     "hledger-vim"     # "anekos/hledger-vim"     
        #   ];
        # }

        # SYSTEM PLUGINS
        # 
        # Haskell, always-on
        # Plug 'ndmitchell/ghcid', { 'rtp': 'plugins/nvim' }
        # Plug 'hspec/hspec.vim'

  
        # " Python
        # " Plug 'nvie/vim-flake8', { 'for': 'python' }
        # " Plug 'vim-scripts/indentpython.vim', { 'for': 'python' }
        # " Plug 'szymonmaszke/vimpyter'
        # 
        # " Automatically executes `filetype plugin indent on` and `syntax enable`.
        # " You can revert the settings after the call. (e.g. filetype indent off, syntax off, etc.)
        # c

        # " Productivity
        # 'wakatime/vim-wakatime'
        # 'jceb/vim-orgmode' | Plug 'mattn/calendar-vim' | Plug 'inkarkat/vim-SyntaxRange' | Plug 'vim-scripts/utl.vim'

      ]; };

      ###########################################################################
      # plug.plugins = with pkgs.vimPlugins; [
      #   youcompleteme
      # ];
      # packages.myVimPackage = with pkgs.vimPlugins; {
      #   # loaded on launch
      #   start = [ LanguageClient-neovim ];
      #   # manually loadable by calling `:packadd $plugin-name`
      #   opt = [];
      #   # To automatically load a plugin when opening a filetype, add vimrc lines like:
      #   # autocmd FileType php :packadd phpCompletion
      # };
      ###########################################################################
      # Vam is SLOOOOOW
      ###########################################################################
      # vam = import ./vam.nix { inherit pkgs; };
      ###########################################################################
      # vam.knownPlugins = (with pkgs.vimPlugints; [ abolish ]);
      #   # (import ./plugins.nix { inherit (pkgs) vimUtils fetchFromGitHub; });
      # vam.pluginDictionaries = {
      #   names = [
      #     "abolish"
      #   ];
      # };
  };
}
