" Delete trailing white space on save
augroup whitespace
  autocmd!
  autocmd BufWrite *.py :call DeleteTrailingWS()
augroup END

" au BufNewFile,BufRead *.py
"     \ set tabstop=4
"     \ set softtabstop=4
"     \ set shiftwidth=4
"     \ set textwidth=79
"     \ set expandtab
"     \ set autoindent
"     \ set fileformat=unix
