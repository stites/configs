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
        # DEFAULTS

        # General {{{
        # Use indentation for folds
        ''
        set foldmethod=indent
        set foldnestmax=5
        set foldlevelstart=99
        set foldcolumn=0
        ''
        ''
        augroup vimrcFold
          " fold rc itself by categories
          autocmd!
          autocmd FileType vim set foldmethod=marker
          autocmd FileType vim set foldlevel=0
        augroup END
        ''
        # With a map leader it's possible to do extra key combinations
        # like <leader>w saves the current file
        (let
          leader = ''\<space>'';
        in
        ''
          if ! exists("mapleader")
            let mapleader = "${leader}"
          endif
          if ! exists("g:mapleader")
            let g:mapleader = "${leader}"
          endif
        '')

        # Leader key timeout
        "set tm=2000"
        # Allow the normal use of "," by pressing it twice
        "noremap ,, ,"
        # Use par for prettier line formatting
        ''
        set formatprg=par
        let $PARINIT = 'rTbgqR B=.,?_A_a Q=_s>|'
        ''

        # Kill the damned Ex mode.
        #nnoremap Q <nop>"

        # Make <c-h> work like <c-h> again (this is a problem with libterm)
        ''
        if has('nvim')
          nnoremap <BS> <C-w>h
        endif
        ''
        # color anything greater than 80 characters as an error
        ''
        highlight OverLength ctermbg=Black guibg=Black
        " match OverLength '\%>81v.\+'
        match OverLength '\%>86v.\+'
        " match ErrorMsg '\%>101v.\+'
        ''
        # When hitting , insert combination of t and spaces for this width.
        # This combination is deleted as if it were 1 t when using backspace.
        "set softtabstop=2"
        # Set code-shifting width. Since smarttab is enabled, this is also the tab
        # insert size for the beginning of a line.
        "set shiftwidth=2"

        # ensure that markdown files and python have a textwidth of 80
        "au BufRead,BufNewFile *.md,*.py setlocal textwidth=120"

        # VIM user interface {{{
        # This is the default in neovim (and might cause problems when set)
        # "set encoding=utf-8"                     # use sane encodings
        "set nocompatible"                         # don't care about vi
        "filetype plugin indent on"                # enable file type detection
        ################################################################################################
        # How do these interact???
        "set smartcase"                            # When searching try to be smart about cases
        "set ignorecase"                           # ignore case on commands and searching
        "set wildignorecase"                       # case-insensitive search
        ################################################################################################
        "set incsearch"                            # Makes search act like search in modern browsers
        "set number" "set relativenumber"          # set hybrid-number-d gutters
        "set autoread"                             # auto-reload files when they are changed (like via git)
        "set history=700"                          # Set lines of history
        "set shell=bash"                           # ensure that we always use bash
        "set tw=120"                               # set textwidth to be 120 globally
        "set hlsearch"                             # Enable search highlighting
        "set mouse=a"                              # Enable mouse in all modes
        "set expandtab"                            # When inserting tab characters, use spaces instead
        "set so=7"                                 # Set 7 lines to the cursor - when moving vertically using j/k
        "set hidden"                               # don't close buffers when you aren't displaying them
        "set wildmenu"                             # Turn on the WiLd menu
        "set wildmode=list:longest,full"           # Tab-complete files up to longest unambiguous prefix
        "set cmdheight=1"                          # Height of the command bar
        "set lazyredraw"                           # Don't redraw while executing macros (good performance config)
        "set magic"                                # For regular expressions turn magic on
        "set showmatch"                            # Show matching brackets when text indicator is over them
        "set mat=2"                                # How many tenths of a second to blink when matching brackets
        "set noerrorbells" "set vb t_vb="          # No annoying sound on errors
        "set termguicolors"                        # use "true color" in the terminal
        "set nobackup" "set nowb" "set noswapfile" # Turn backup off, since most stuff is in Git anyway...
        "set expandtab"                            # Use spaces instead of tabs
        "set smarttab"                             # Be smart when using tabs ;)
        "set shiftwidth=2" "set tabstop=2"         # 1 tab == 2 spaces
        "set lbr" "set tw=500"                     # Linebreak on 500 characters
        "set ai"                                   # Auto indent
        "set si"                                   # Smart indent
        "set wrap"                                 # Wrap lines
        "set ffs=unix,dos,mac"                     # Use Unix as the standard file type
        "set laststatus=2"                         # Always show the status line
        "set list"                                 # Show trailing whitespace

        # "set nohlsearch"                           # <<<<< actually don't highlight ??????
        # And never type :nohlsearch again!!!
        "nnoremap <esc><esc> :noh<return><esc>"
        # "nnoremap <silent> <esc> <esc>:silent :nohlsearch<cr>"

        # ..but only interesting whitespace
        ''
        if &listchars ==# 'eol:$'
          set listchars=tab:>\ ,trail:-,extends:>,precedes:<,nbsp:+
        endif
        ''

        # Configure backspace so it acts as it should act
        "set backspace=eol,start,indent"
        "set whichwrap+=<,>,h,l"

        # disable Background Color Erase (BCE) so that color schemes
        # render properly when inside 256-color tmux and GNU screen.
        # see also http://snk.tuxfamily.org/log/vim-256color-bce.html
        ''
        if &term =~ '256color'
          set t_ut=
        endif
        ''

        # Force redraw
        "map <silent> <leader>r :redraw!<CR>"

        # Turn mouse mode on
        "nnoremap <leader>ma :set mouse=a<cr>"

        # Turn mouse mode off
        "nnoremap <leader>mo :set mouse=<cr>"


        # Read coco format as python (coco is "functional python")
        "au! BufNewFile,BufRead *.coco set filetype=python"

        "tnoremap <Esc> <C-\\><C-n>"
        "set encoding=utf-8"

        ''let g:gitgutter_git_executable="${pkgs.git}/bin/git"''

        # (builtins.readFile ./defaults.vim     )
        (builtins.readFile ./haskell.vim      )
        # (builtins.readFile ./elm.vim          )
        (builtins.readFile ./plugins-config.vim)
        # (builtins.readFile ./spelling.vim     )
        # (builtins.readFile ./ui.vim       )
        # (builtins.readFile ./python.vim       )

        # Spelling {{{
        ''
        abbr Lunix Linux
        abbr accross across
        abbr hte the
        abbr Probablistic Probabilistic
        ''
        # }}}

        # Colors and Fonts {{{
        ''
        try
          colorscheme wombat256mod

          " https://sunaku.github.io/vim-256color-bce.html
          " perma: https://perma.cc/Q6GC-ZD4A
          set term=screen-256color
        catch
        endtry
        ''

        # ''
        # " " Adjust signscolumn to match wombat
        # " hi! link SignColumn LineNr
        # "
        # " " Use pleasant but very visible search hilighting
        # " hi Search ctermfg=white ctermbg=173 cterm=none guifg=#ffffff guibg=#e5786d gui=none
        # " hi! link Visual Search
        # "
        # " " Match wombat colors in nerd tree
        # " hi Directory guifg=#8ac6f2
        # "
        # " " Searing red very visible cursor
        # " hi Cursor guibg=red
        # ''

        # Don't blink normal mode cursor
        "set guicursor=n-v-c:block-Cursor"
        "set guicursor+=n-v-c:blinkon0"

        # only work in 256 colors
        "set t_Co=256"

        # Set utf8 as standard encoding and en_US as the standard language
        # ''
        # if !has('nvim')
        #   " Only set this for vim, since neovim is utf8 as default and setting it
        #   " causes problems when reloading the .vimrc configuration
        #   set encoding=utf8
        # endif
        # ''


        # Use powerline fonts for airline
        ''
        if !exists('g:airline_symbols')
          let g:airline_symbols = {}
        endif

        let g:airline_powerline_fonts = 1
        let g:airline_symbols.space = "\ua0"
        ''


        ''
        function! ProseMode()
          call goyo#execute(0, [])
          set spell noci nosi noai nolist noshowmode noshowcmd
          set complete+=s
          set bg=light
          " if !has('gui_running')
          "   let g:solarized_termcolors=256
          " endif
          " colors solarized
        endfunction

        command! ProseMode call ProseMode()
        nmap \p :ProseMode<CR>
        ''

        ''
        let g:vimtex_compiler_progname = 'nvr'
        let @x=':%s/<Plug>_*//g'
        ''

        # Files, backups and undo {{{

        ## USE LESSSPACE INSTEAD!
        ## # strip trailing whitespace everywhere
        ## ''
        ## fun! <SID>StripTrailingWhitespaces()
        ##     let l = line(".")
        ##     let c = col(".")
        ##     %s/\s\+$//e
        ##     call cursor(l, c)
        ## endfun
        ## autocmd FileType * autocmd BufWritePre <buffer> :call <SID>StripTrailingWhitespaces()
        ## " autocmd FileType * autocmd BufWritePre <buffer> :call <SID>StripTrailingWhitespaces()
        ## ''
        ## # autocmd FileType haskell,c,cpp,java,php,ruby,python,markdown autocmd BufWritePre <buffer> :call <SID>StripTrailingWhitespaces()

        ## # # ALTERNATIVE: Utility function to delete trailing white space
        ## # ''
        ## # fun! DeleteTrailingWS()
        ## #   exe "normal mz"
        ## #   %s/\s\+$//ge
        ## #   exe "normal `z"
        ## # endfun
        ## # " autocmd BufWritePre * :call DeleteTrailingWS()
        ## # ''
        ## # }}}

        ''
        " au BufNewFile,BufRead *.py
        "     \ set tabstop=4
        "     \ set softtabstop=4
        "     \ set shiftwidth=4
        "     \ set textwidth=79
        "     \ set expandtab
        "     \ set autoindent
        "     \ set fileformat=unix
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

        # Open file prompt with current path
        ''nmap <leader>e :e <C-R>=expand("%:p:h") . '/'<CR>''

        # Show undo tree
        "nmap <silent> <leader>u :MundoToggle<CR>"

        # Fuzzy find files
        ''
        nnoremap <silent> <Leader><space> :CtrlP<CR>
        let g:ctrlp_max_files=0
        let g:ctrlp_show_hidden=1
        let g:ctrlp_custom_ignore = { 'dir': '\v[\/](.git|.cabal-sandbox|.stack-work)$' }
        ''

        # Allow saving of files as sudo when I forgot to start vim using sudo.
        "cmap w!! w !sudo tee > /dev/null %"

        # Text, tab and indent related {{{

        # Copy and paste to os clipboard
        ''
        nmap <leader>y "+y
        vmap <leader>y "+y
        nmap <leader>d "+d
        vmap <leader>d "+d
        nmap <leader>p "+p
        vmap <leader>p "+p
        ''

        # Visual mode related {{{

        # Visual mode pressing * or # searches for the current selection
        # Super useful! From an idea by Michael Naumann
        ''
        vnoremap <silent> * :call VisualSelection('f', '''''')<CR>
        vnoremap <silent> # :call VisualSelection('b', '''''')<CR>
        ''
        # }}}

        # Moving around, tabs, windows and buffers {{{

        # Treat long lines as break lines (useful when moving around in them)
        "nnoremap j gj"
        "nnoremap k gk"

        "noremap <c-h> <c-w>h"
        "noremap <c-k> <c-w>k"
        "noremap <c-j> <c-w>j"
        "noremap <c-l> <c-w>l"

        # Disable highlight when <leader><cr> is pressed
        # but preserve cursor coloring
        ''
        nmap <silent> <leader><cr> :noh\|hi Cursor guibg=red<cr>

        " Return to last edit position when opening files (You want this!)
        augroup last_edit
          autocmd!
          autocmd BufReadPost *
               \ if line("'\"") > 0 && line("'\"") <= line("$") |
               \   exe "normal! g`\"" |
               \ endif
        augroup END
        ''
        # Remember info about open buffers on close
        "set viminfo^=%"

        # Open window splits in various places
        "nmap <leader>sh :leftabove  vnew<CR>"
        "nmap <leader>sl :rightbelow vnew<CR>"
        "nmap <leader>sk :leftabove  new<CR>"
        "nmap <leader>sj :rightbelow new<CR>"

        # Manually create key mappings (to avoid rebinding C-\)
        "let g:tmux_navigator_no_mappings = 1"

        "nnoremap <silent> <C-h> :TmuxNavigateLeft<cr>"
        "nnoremap <silent> <C-j> :TmuxNavigateDown<cr>"
        "nnoremap <silent> <C-k> :TmuxNavigateUp<cr>"
        "nnoremap <silent> <C-l> :TmuxNavigateRight<cr>"

        # previous buffer, next buffer
        "nnoremap <leader>bp :bp<cr>"
        "nnoremap <leader>bn :bn<cr>"

        # close every window in current tabview but the current
        "nnoremap <leader>bo <c-w>o"

        # delete buffer without closing pane
        "noremap <leader>bd :Bd<cr>"

        # fuzzy find buffers
        "noremap <leader>b<space> :CtrlPBuffer<cr>"


        # Neovim terminal configurations
        ''
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

        # Status line {{{


        # }}}

        # Editing mappings {{{

        # Spell checking {{{
        # Pressing ,ss will toggle and untoggle spell checking
        ''
        map <leader>ss :setlocal spell!<cr>
        ''
        # }}}

        # Helper functions {{{
        ''
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
        ''
        # }}}

        # Slime {{{
        ''
        vmap <silent> <Leader>rs <Plug>SendSelectionToTmux
        nmap <silent> <Leader>rs <Plug>NormalModeSendToTmux
        nmap <silent> <Leader>rv <Plug>SetTmuxVars
        ''

        # }}}

        # Alignment {{{

        # Stop Align plugin from forcing its mappings on us
        "let g:loaded_AlignMapsPlugin=1"
        # Align on equal signs
        "map <Leader>a= :Align =<CR>"
        # Align on commas
        "map <Leader>a, :Align ,<CR>"
        # Align on pipes
        "map <Leader>a<bar> :Align <bar><CR>"
        # Prompt for align character
        "map <leader>ap :Align"

        # }}}

        # Tags {{{

        "map <leader>tt :TagbarToggle<CR>"

        "set tags+=./tags;$HOME,./codex.tags;$HOME"
        "set cst"
        "set csverb"

        # IGNORES ARE HERE BECAUSE THEY INTERFERE WITH CTAG LOOKUP
        "set wildignore+=*.min.*"       # Web ignores
        "set wildignore+=*.stack-work*" # Haskell ignores
        "set wildignore+=*.so"          # C ignores

        # Python ignores
        ''
        set wildignore+=__pycache__/*,*.py[cod],*$py.class,*.ipynb,.Python,env/*,build/*
        set wildignore+=develop-eggs/*,dist/*,downloads/*,eggs/*,.eggs/*,lib/*,lib64/*
        set wildignore+=parts/*,sdist/*,var/*,*.egg-info/*,.installed.cfg,*.egg,*.manifest
        set wildignore+=*.spec,pip-log.txt,pip-delete-this-directory.txt,htmlcov/*
        set wildignore+=__pycache__/*,.tox/*,.coverage,.coverage.*,.cache,nosetests.xml
        set wildignore+=coverage.xml,cover,.hypothesis/*,*.mo,*.pot,*.log,local_settings.py
        set wildignore+=instance/*,.webassets-cache,.scrapy,docs/_build/*,target/*
        set wildignore+=.ipynb_checkpoints,.python-version,celerybeat-schedule,.env,venv/*
        set wildignore+=ENV/*,.spyderproject,.ropeproject,.DS_Store,*.sublime-workspace
        ''

        # }}}

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

        # LanguageClient-neovim ========================================================
        # set rtp+=~/.config/nvim/bundle/LanguageClient-neovim//pack/XXX/start/LanguageClient-neovim
        # set rtp+=~/.nix-profil/bin/
        # set runtimepath+=$HOME/.nix-profile/bin

        # Required for operations modifying multiple buffers like rename.
        "set hidden"
        "let g:LanguageClient_loadSettings=1"
        # Use an absolute configuration path if you want system-wide settings
        "let g:LanguageClient_settingsPath='/home/stites/.config/nvim/lsp_settings.json'"

        # https://github.com/autozimu/LanguageClient-neovim/issues/379 LSP snippet is not supported
        "let g:LanguageClient_hasSnippetSupport = 0"
        # "let g:LanguageClient_devel = 1" Use rust debug build

        "let g:LanguageClient_serverCommands = {"
        "    \\ 'haskell': ['hie-wrapper', '--lsp'],"
        "    \\ 'haskell.hspec': ['hie-wrapper', '--lsp'],"
        "    \\ 'python': ['pyls'],"  # use -vv for debug mode
        "    \\ 'c':    ['clangd'],"  #['ccls', '--log-file=/tmp/cc.log'],"      # could also be 'clangd' or just 'ccls'
        "    \\ 'cpp':  ['clangd'],"  #['ccls', '--log-file=/tmp/cc.log'],"
        "    \\ 'cuda': ['clangd'],"  #['ccls', '--log-file=/tmp/cc.log'],"
        "    \\ 'objc': ['clangd']"   #['ccls', '--log-file=/tmp/cc.log']"
        "    \\ }"

        # OTHER LSPs:
        # "    \\ 'rust': ['rustup', 'run', 'stable', 'rls'],"
        # "    \\ 'javascript': ['javascript-typescript-stdio'],"
        # "    \\ 'javascript.jsx': ['tcp://127.0.0.1:2089'],"
        # "    \\ 'sh': ['bash-language-server', 'start']
        # " html
        # " purescript
        # " typescript
        # " yaml

        # let g:LanguageClient_loggingLevel = 'DEBUG' " Use highest logging level
        #
        # If you're finding that the server isn't starting at the correct project root, it may also be helpful to also specify root markers:
        # let g:LanguageClient_rootMarkers = ['*.cabal', 'stack.yaml', 'cabal.project', 'requirements.txt', 'setup.py']
        ''
        let g:LanguageClient_windowLogMessageLevel = "Warning"
        let g:LanguageClient_hoverPreview = "Always" " \"Never\", \"Auto\", \"Always\"
        ''


        # You'll probably want to add some mappings for common commands:
        # If you'd like diagnostics to be highlighted, add a highlight group for ALEError/ALEWarning/ALEInfo,
        # or customize g:LanguageClient_diagnosticsDisplay:
        ''
        hi link ALEError Error
        hi Warning term=underline cterm=underline ctermfg=Yellow gui=undercurl guisp=Gold
        hi link ALEWarning Warning
        hi link ALEInfo SpellCap
        ''

        # function LC_maps()
        #   if has_key(g:LanguageClient_serverCommands, &filetype)
          # Format - gq => same as vim formatting!
          "set formatexpr=LanguageClient#textDocument_rangeFormatting_sync()"

          # ??? - K => Show type info (and short doc) of identifier under cursor.
          "nnoremap <buffer> <silent> K    :call LanguageClient#textDocument_hover()<CR>"

          # Error - E => Show detailed error under cursorr
          "nnoremap <buffer> <silent> E    :call LanguageClient#explainErrorAtPoint()<CR>"

          # Goto Definition - gd => go to the definition of the symbol under the cursor
          "nnoremap <buffer> <silent> gd   :call LanguageClient#textDocument_definition()<CR>"

          # Highlight - h => Highlight usages of the symbol under the cursor.
          "noremap <Leader>h :call LanguageClient#clearDocumentHighlight()<CR>"

          # Highlights, Clear - hc => Highlight usages of the symbol under the cursor.
          "noremap <Leader>hc :call LanguageClient#clearDocumentHighlight()<CR>"

          # ??? - F2 => rename with prompt
          "nnoremap <buffer> <silent> <F2> :call LanguageClient#textDocument_rename()<CR>"

          # Rename - rn => rename
          "noremap <Leader>rn :call LanguageClient#textDocument_rename()<CR>"

          # Rename - rc => rename camelCase
          "noremap <leader>rc :call LanguageClient#textDocument_rename({'newName': Abolish.camelcase(expand('<cword>'))})<CR>"

          # Rename - rs => rename snake_case
          "noremap <leader>rs :call LanguageClient#textDocument_rename({'newName': Abolish.snakecase(expand('<cword>'))})<CR>"

          # Rename - ru => rename UPPERCASE
          "noremap <leader>ru :call LanguageClient#textDocument_rename({'newName': Abolish.uppercase(expand('<cword>'))})<CR>"

          # List References - lr => List all references of identifier under cursor.
          "noremap <Leader>lr :call LanguageClient#textDocument_references()<CR>"

          # List Actions - la => Show code actions at current location.
          "noremap <Leader>la :call LanguageClient#textDocument_codeAction()<CR>"

          # List Symbols - ls => show current buffer's symbols
          "noremap <Leader>ls :call LanguageClient#textDocument_documentSymbol()<CR>"

          # Context Menu - <F5> => show the LSP menu
          "nnoremap <F5> :call LanguageClient_contextMenu()<CR>"
        #   endif
        # endfunction
        # autocmd FileType * call LC_maps()

        "let g:chromatica#libclang_path='${pkgs.llvm_7}/lib'"
        "let g:chromatica#enable_at_startup=1"
      ];

      ###########################################################################
      # vim-plug automatically executes `filetype plugin indent on` and `syntax enable`.
      # You can revert the settings after the call. (e.g. filetype indent off, syntax off, etc.)
      plug.plugins = (with pkgs.vimPlugins; [
        # for more: https://github.com/mhinz/vim-galore/blob/master/PLUGINS.md#c-and-c

        # # C++ development
        customPlugins.chromatica-nvim
        # clang_complete # Plug 'Rip-Rip/clang_complete', { 'for': 'c' }
        deoplete-clang

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
        tabular                 # 'godlygeek/tabular'

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
        vim-indent-object       # 'michaeljsmith/vim-indent-object'
        vim-easymotion          # 'easymotion/vim-easymotion'
        customPlugins.vim-bracketed-paste # 'ConradIrwin/vim-bracketed-paste'
        customPlugins.vim-sandwich   # a simpler version of vim-surround: sa_textblock_<char> or sdb/sd<char>. sdb searches for the char automatically
        # vim-surround               # 'tpope/vim-surround'   " http://vimawesome.com/plugin/surround-vim
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

        customPlugins.lessspace-vim         # Trim trailing whitespace only on lines you edit (or visit in insert mode)
        customPlugins.vim-highlightedyank   # highlight what you yank!

      ]);
    };
  };
}
