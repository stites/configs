" enable spell checking
" autocmd BufRead,BufNewFile *.md setlocal spell spelllang=en_us
" autocmd FileType gitcommit setlocal spell spelllang=en_us

let g:ctrlp_user_command = 'ag %s -l --nocolor --hidden -g ""'
let g:ctrlp_max_files=0
let g:ctrlp_show_hidden=1
let g:ctrlp_custom_ignore =
  \ {
  \   'dir': '\v[\/](.git|.cabal-sandbox|.stack-work)$',
  \   'file': '\v\.(o|hi|beam|dyn_hi|dyn_o)$'
  \ }

let g:haskell_tabular = 1

vmap a= :Tabularize /=<CR>
vmap a; :Tabularize /::<CR>
vmap a- :Tabularize /-><CR>
vmap am :Tabularize / as<CR>
vmap a, :Tabularize /,<CR>

" setlocal spell spelllang=en_us

" pep8 for python
" au BufNewFile,BufRead *.py
"     \ set tabstop=4
"     \ set softtabstop=4
"     \ set shiftwidth=4
"     \ set textwidth=79
"     \ set expandtab
"     \ set autoindent
"     \ set fileformat=unix

" call vim-flake8 on every file save
"autocmd BufWritePost *.py call Flake8()

" ==============================================================================
" vim-snipmate
" noremap <C-t><Tab> snipMateTrigger


" " ==============================================================================
" " vim-easymotion
" map  <Leader>f <Plug>(easymotion-bd-f)
" nmap <Leader>f <Plug>(easymotion-overwin-f)
" nmap F <Plug>(easymotion-overwin-f2)
"
" " Turn on case insensitive feature
" let g:EasyMotion_smartcase = 1

" ==============================================================================
" https://github.com/sol/hpack
" run hpack automatically on modifications to package.yaml
autocmd BufWritePost package.yaml silent !hpack --silent

" Add these to your vimrc to automatically keep the tags file up to date.
" Unfortunately silent means the errors look a little ugly, I suppose I could
" capture those and print them out with echohl WarningMsg.
au BufWritePost *.hs  silent !codex update --force %
au BufWritePost *.hsc silent !codex update --force %

" ==============================================================================
" disable haskell indents
let g:haskell_indent_disable=1

" ==============================================================================
" Use deoplete.
let g:deoplete#enable_at_startup = 1
" disable autocomplete
" let g:deoplete#disable_auto_complete = 1
" inoremap <silent><expr><C-Space> deoplete#mappings#manual_complete()

" ==============================================================================
" UltiSnips config
inoremap <silent><expr><TAB> pumvisible() ? "\<C-n>" : "\<TAB>"
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<tab>"
let g:UltiSnipsJumpBackwardTrigger="<s-tab>"

" ==============================================================================
" neomake
autocmd! BufWritePost *.hs Neomake
let g:neomake_haskell_hlint_maker = {
    \ 'args': ['--verbose'],
    \ 'errorformat': '%A%f: line %l\, col %v\, %m \(%t%*\d\)',
    \ }
let g:neomake_haskell_enabled_makers = ['hlint']

" ==============================================================================
" vim-session
" https://peterodding.com/code/vim/session/
" https://perma.cc/E487-SS2S
let g:session_autosave_periodic = 30   " minutes
let g:session_directory = "/home/stites/.vim/session/"
let g:session_lock_directory = "/home/stites/.vim/session-locks/"

" Disable all session locking - I know what I'm doing :-).
" let g:session_lock_enabled = 0
