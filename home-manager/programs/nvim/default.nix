{ lib, pkgs, stdenv, fetchgit, vimUtils, ... }:
let
  customPlugins = pkgs.callPackage ./plugins.nix {};
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

        " use "true color" in the terminal
        set termguicolors

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

      ###########################################################################
      # vim-plug automatically executes `filetype plugin indent on` and `syntax enable`.
      # You can revert the settings after the call. (e.g. filetype indent off, syntax off, etc.)
      plug.plugins = (with pkgs.vimPlugins; [
        # # C++ development
        clighter8
        clang_complete # Plug 'Rip-Rip/clang_complete', { 'for': 'c' }

        # Dhall
        dhall-vim # Plug 'vmchale/dhall-vim',     { 'for': 'dhall' }

        # Haskell
        haskell-vim                # Plug 'neovimhaskell/haskell-vim',     { 'for': 'haskell' }
        vim-hoogle                 # Plug 'Twinside/vim-hoogle',           { 'for': 'haskell' }
        hlint-refactor-vim         # Plug 'mpickering/hlint-refactor-vim', { 'for': 'haskell' }
        neco-ghc                   # Plug 'eagletmt/neco-ghc',             { 'for': 'haskell' }
        customPlugins.vim-brittany # Plug 'meck/vim-brittany',             { 'for': 'haskell' }

        # Haskell, always-on
        customPlugins.ghcid
        customPlugins.hspec-vim

        # HTML, XML
        customPlugins.closetag-vim             # Plug 'docunext/closetag.vim', { 'for': 'html' }

        # # elm - elm-vim MUST be before vim-polyglot
        # elm-vim # Plug 'ElmCast/elm-vim', { 'for': 'elm' }

        # # ruby
        # customPlugins.vim-endwise     # Plug 'tpope/vim-endwise', { 'for': 'ruby' }

        # LaTeX plugins
        vimtex            # Plug 'lervag/vimtex', { 'for': 'latex' }

        # # HLedger
        customPlugins.hledger-vim # Plug 'anekos/hledger-vim', { 'for': 'journal' }

        # Python
        # Plug 'nvie/vim-flake8', { 'for': 'python' }
        # Plug 'vim-scripts/indentpython.vim', { 'for': 'python' }
        # Plug 'szymonmaszke/vimpyter'

        # rust
        rust-vim # syntax highlighting

        # nix
        vim-nix # syntax highlighting

        # Productivity
        customPlugins.utl-vim
        vim-SyntaxRange
        calendar-vim
        vim-orgmode # 'jceb/vim-orgmode' | Plug 'mattn/calendar-vim' | Plug 'inkarkat/vim-SyntaxRange' | Plug 'vim-scripts/utl.vim'

        ##############################################
        # IDE support
        ##############################################
        LanguageClient-neovim   # specifying the build is handled in nix
        vim-abolish             # Use with LSP to search for, substitute, and abbreviate multiple variants of a word
        vim-multiple-cursors    # multicursors
        vim-hier                # highlight errors
        vim-polyglot            # handle the rest of the languages
        rainbow_parentheses-vim # make parens pretty

        # alignment stuff
        align                   # align characters
        vim-easy-align          # more comprehensive alignment???

        # git compatability
        vim-fugitive            # compatability with git
        vim-rhubarb             # compatability with github (depends on fugitive)
        vim-gitgutter           # show git changes in the gutter

        ##############################################
        # Colorschemes, layouts, and icons
        ##############################################
        vim-airline                          # add statusbar
        vim-airline-themes                   # theme statusbar
        vim-devicons                         # add unknown icons for statusbar
        gruvbox                              # [?????] retro theme
        papercolor-theme                     # [?????]
        wombat256-vim                        # [dark]  haskell favorite theme
        customPlugins.vim-github-colorscheme # [light] github colorscheme
        customPlugins.oceanic-next           # [?????]
        customPlugins.vim-kalisi             # [?????]
        customPlugins.yaflandia              # [?????] c++ theme???

        ######################################
        # Writing support
        ######################################
        goyo-vim                # enter "writing mode"
        limelight-vim           # highlight cursor's text object (paragraph) when in writing mode
        vim-grammarous          # grammar checking

        ######################################
        # Bars, panels, and files
        ######################################
        fzfWrapper
        fzf-vim          # Plug 'junegunn/fzf.vim' | Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
        ack-vim          # file search
        tagbar           # http://vimawesome.com/plugin/tagbar

        # Text manipulation
        vim-sensible            # make vim sane
        vim-unimpaired          # pairs of handy bracket mappings
        customPlugins.vim-mundo # 'simnalamburt/vim-mundo'
        vim-commentary          # 'tpope/vim-commentary'
        tabular                 # 'godlygeek/tabular'
        vim-indent-object       # 'michaeljsmith/vim-indent-object'
        vim-easymotion          # 'easymotion/vim-easymotion'
        customPlugins.vim-bracketed-paste # 'ConradIrwin/vim-bracketed-paste'
        vim-surround            # 'tpope/vim-surround'   " http://vimawesome.com/plugin/surround-vim
        vim-repeat              # 'tpope/vim-repeat'     " to be used with vim-surround
        vim-speeddating         # 'tpope/vim-speeddating'
        syntastic               # 'vim-syntastic/syntastic'
        vim-github-dashboard    # 'junegunn/vim-github-dashboard'
        customPlugins.vim-swoop # 'pelodelfuego/vim-swoop'

        # ?????????? is this a web version of vim???
        # "webapi-vim"

        # support
        vimproc       # Plug 'Shougo/vimproc.vim', { 'do': 'vim_gmake' }
        neomake
        customPlugins.vim-bbye
        vim-indent-guides
        ultisnips                          # snippets
        deoplete-nvim                      # Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
        customPlugins.terminalkeys-vim     # Plug 'nacitar/terminalkeys.vim'
        # customPlugins.vim-misc
        # customPlugins.vim-session          # Plug 'xolox/vim-session' | Plug 'xolox/vim-misc'
        customPlugins.vim-bufkill          # Plug 'qpkorr/vim-bufkill'
        vim-eunuch                         # Plug 'tpope/vim-eunuch'
        undotree                           # Plug 'mbbill/undotree'

        # tmux integration
        vim-tmux-navigator                 # Allow pane movement to jump out of vim into tmux
        tslime                             # send commands from vim to a running tmux session

        # Vim improvements:
        customPlugins.vim-textobj-sentence
        customPlugins.vim-textobj-user           # Plug 'kana/vim-textobj-user' | Plug 'reedes/vim-textobj-sentence'


      ]);
    };
  };
}
