{ pkgs, lib, ... }:
{
  extraConfig = lib.strings.concatStringsSep "\n" [
    # DEFAULTS

    # General {{{
    # Use indentation for folds
    ''
    set foldmethod=indent
    set foldnestmax=5
    set foldlevelstart=99
    set foldcolumn=0
    ''
    # we don't need this anymore since we are working in nix!
    # ''
    # augroup vimrcFold
    #   " fold rc itself by categories
    #   autocmd!
    #   autocmd FileType vim set foldmethod=marker
    #   autocmd FileType vim set foldlevel=0
    # augroup END
    # ''
    # With a map leader it's possible to do extra key combinations
    # like <leader>w saves the current file
    (let leader = ''\<space>'';
      in lib.strings.concatStringsSep "\n" ([
        ''
          if ! exists("mapleader")
            let mapleader = "${leader}"
          endif
          if ! exists("g:mapleader")
            let g:mapleader = "${leader}"
          endif
        ''
      # Allow the normal use of "," by pressing it twice
      ] ++ lib.optional (leader == ",") ["noremap ,, ,"])
    )

    # Leader key timeout
    "set tm=2000"
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
    "set number relativenumber"                # set hybrid-number-d gutters
    "set autoread"                             # auto-reload files when they are changed (like via git)
    "set history=700"                          # Set lines of history
    "set shell=bash"                           # ensure that we always use bash
    "set tw=120"                               # set textwidth to be 120 globally
    "set hlsearch"                             # Enable search highlighting
    "set mouse=a"                              # Enable mouse in all modes
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
    "set listchars=tab:▸▸,trail:·"             # Show `▸▸` for tabs: 	, `·` for tailing whitespace
    "set clipboard=unnamed"                    # Use the system clipboard

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

    "tnoremap <Esc> <C-\\><C-n>"
    "set encoding=utf-8"

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

    # Visual mode related {{{

    # Visual mode pressing * or # searches for the current selection
    # Super useful! From an idea by Michael Naumann

    ''
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

    vnoremap <silent> * :call VisualSelection('f', '''''')<CR>
    vnoremap <silent> # :call VisualSelection('b', '''''')<CR>
    ''
    # }}}

    # Spell checking {{{
    # Pressing ,ss will toggle and untoggle spell checking
    ''
    map <leader>ss :setlocal spell!<cr>
    ''
    # }}}

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
      # Remember info about open buffers on close
      "set viminfo^=%"

      # Treat long lines as break lines (useful when moving around in them)
      "nnoremap j gj"
      "nnoremap k gk"

      "noremap <c-h> <c-w>h"
      "noremap <c-k> <c-w>k"
      "noremap <c-j> <c-w>j"
      "noremap <c-l> <c-w>l"

      # Open file prompt with current path
      ''nmap <leader>e :e <C-R>=expand("%:p:h") . '/'<CR>''

      # Tags {{{

      "set tags+=./tags;$HOME,./codex.tags;$HOME"
      "set cst"
      "set csverb"

      # IGNORES ARE HERE BECAUSE THEY INTERFERE WITH CTAG LOOKUP
      "set wildignore+=*.min.*"       # Web ignores
      "set wildignore+=*.stack-work*" # Haskell ignores
      "set wildignore+=*.so"          # C ignores

      # }}}

    # Spelling
    ''
    abbr Lunix Linux
    abbr accross across
    abbr hte the
    abbr Probablistic Probabilistic
    ''
  ];
}
