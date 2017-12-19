" General {{{
" Use indentation for folds
set foldmethod=indent
set foldnestmax=5
set foldlevelstart=99
set foldcolumn=0

augroup vimrcFold
  " fold rc itself by categories
  autocmd!
  autocmd FileType vim set foldmethod=marker
  autocmd FileType vim set foldlevel=0
augroup END

" With a map leader it's possible to do extra key combinations
" like <leader>w saves the current file
if ! exists("mapleader")
  let mapleader = ","
endif

if ! exists("g:mapleader")
  let g:mapleader = ","
endif

" Leader key timeout
set tm=2000

" Allow the normal use of "," by pressing it twice
noremap ,, ,

" Use par for prettier line formatting
set formatprg=par
let $PARINIT = 'rTbgqR B=.,?_A_a Q=_s>|'

" Kill the damned Ex mode.
nnoremap Q <nop>

" Make <c-h> work like <c-h> again (this is a problem with libterm)
if has('nvim')
  nnoremap <BS> <C-w>h
endif

" color anything greater than 80 characters as an error
highlight OverLength ctermbg=Black guibg=Black
" match OverLength '\%>81v.\+'
match OverLength '\%>86v.\+'
" match ErrorMsg '\%>101v.\+'

" When hitting , insert combination of t and spaces for this width.
" This combination is deleted as if it were 1 t when using backspace.
set softtabstop=2

" Set code-shifting width. Since smarttab is enabled, this is also the tab
" insert size for the beginning of a line.
set shiftwidth=2

" ensure that markdown files have a textwidth of 80
" au BufRead,BufNewFile *.md setlocal textwidth=80

" ==============================================================================
" Tab navigation like Firefox.
" ----------------------------
" no need! i don't use tabs

" nnoremap <C-S-Tab> :tabprevious<CR>
" nnoremap <C-Tab>   :tabnext<CR>
" inoremap <C-S-Tab> <Esc>:tabprevious<CR>i
" inoremap <C-Tab>   <Esc>:tabnext<CR>i
" nnoremap <C-t>     :tabnew<CR>
" inoremap <C-t>     <Esc>:tabnew<CR>
" nnoremap <C-w>     :tabclose<CR>

" }}}


" VIM user interface {{{
set nocompatible            " do not care about vi
filetype plugin indent on   " enable file type detection
set ignorecase              " ignore case on commands
set relativenumber          " set hybrid-number-d gutters
set number
set encoding=utf-8          " use sane encodings
set autoread                " auto-reload files when they are changed (like via git)
set history=700             " Set lines of history
set shell=/bin/bash         " ensure that we always use bash
set tw=80                   " set textwidth to be 80 globally
set hlsearch                " Enable search highlighting
set mouse=a                 " Enable mouse in all modes
set expandtab               " When inserting tab characters, use spaces instead
set so=7                    " Set 7 lines to the cursor - when moving vertically using j/k
set wildmenu                   " Turn on the WiLd menu
set wildmode=list:longest,full " Tab-complete files up to longest unambiguous prefix
set list                    " Show trailing whitespace

" But only interesting whitespace
if &listchars ==# 'eol:$'
  set listchars=tab:>\ ,trail:-,extends:>,precedes:<,nbsp:+
endif

" Height of the command bar
set cmdheight=1

" Configure backspace so it acts as it should act
set backspace=eol,start,indent
set whichwrap+=<,>,h,l

" Ignore case when searching
set ignorecase

" When searching try to be smart about cases
set smartcase

" Highlight search results
set hlsearch

" Makes search act like search in modern browsers
set incsearch

" Don't redraw while executing macros (good performance config)
set lazyredraw

" For regular expressions turn magic on
set magic

" Show matching brackets when text indicator is over them
set showmatch

" How many tenths of a second to blink when matching brackets
set mat=2

" No annoying sound on errors
set noerrorbells
set vb t_vb=

if &term =~ '256color'
  " disable Background Color Erase (BCE) so that color schemes
  " render properly when inside 256-color tmux and GNU screen.
  " see also http://snk.tuxfamily.org/log/vim-256color-bce.html
  set t_ut=
endif

" Force redraw
map <silent> <leader>r :redraw!<CR>

" Turn mouse mode on
nnoremap <leader>ma :set mouse=a<cr>

" Turn mouse mode off
nnoremap <leader>mo :set mouse=<cr>

" Default to mouse mode on
set mouse=a

" Change cursor shape between insert and normal mode in iTerm2.app
if $TERM_PROGRAM =~ "iTerm"
  let &t_SI = "\<Esc>]50;CursorShape=1\x7" " Vertical bar in insert mode
  let &t_EI = "\<Esc>]50;CursorShape=0\x7" " Block in normal mode
endif
" }}}


