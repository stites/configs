" Delete trailing white space on save
augroup whitespace
  autocmd!
  autocmd BufWrite *.py :call DeleteTrailingWS()
augroup END

" Don't do anything smart for python indentations (otherwise comments drop to col-0)
au! FileType python setl nosmartindent

" au BufNewFile,BufRead *.py
"     \ set tabstop=4
"     \ set softtabstop=4
"     \ set shiftwidth=4
"     \ set textwidth=79
"     \ set expandtab
"     \ set autoindent
"     \ set fileformat=unix
