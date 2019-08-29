{ lib, pkgs, ... }:
{
  plugins = with pkgs.vimPlugins; [
    # git compatability
    vim-fugitive            # compatability with git
    vim-rhubarb             # compatability with github (depends on fugitive)
    vim-gitgutter           # show git changes in the gutter
  ];
  rc = lib.strings.concatStringsSep "\n" [
    ''let g:gitgutter_git_executable="${pkgs.git}/bin/git"''
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
    ''
  ];
}
