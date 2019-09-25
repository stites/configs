{ pkgs, ... }:
let
  keybindings = (pkgs.callPackage ../../keybindings {});
  leader = keybindings.leader;
  usetabnine = true;
in
with keybindings.lib;
{
  pkg = pkgs.vimPlugins.coc-nvim;
  priority = 100;

  # TODO: need a way to specify reverse dependencies or reorder priorities
  dependencies = [
    # use out-of-the-box airline integration
    ../vim-airline.nix
    # ./highlight.nix
    # ./yank.nix
    # ./snippets.nix
    # ./lists.nix
  # ] ++ (if usetabnine then [./tabnine.nix] else [
    # # ./neco.nix
    # # ./html.nix
    # ./python.nix
    # ./vimtex.nix
  # ]);
  ];
  coc-settings = {
    "suggest.timeout" = 500;
  };
  extraConfig = [
    # if hidden is not set, TextEdit might fail.
    "set hidden"

    # Some servers have issues with backup files, see #649
    "set nobackup nowritebackup"

    # Better display for messages
    "set cmdheight=2"

    # You will have bad experience for diagnostic messages when it's default 4000.
    "set updatetime=300"

    # don't give |ins-completion-menu| messages.
    "set shortmess+=c"

    # always show signcolumns
    "set signcolumn=yes"

    # Use tab for trigger completion with characters ahead and navigate.
    # Use command ':verbose imap <tab>' to make sure tab is not mapped by other plugin.
    ''
    inoremap <silent><expr> <TAB>
          \ pumvisible() ? "\<C-n>" :
          \ <SID>check_back_space() ? "\<TAB>" :
          \ coc#refresh()
    inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

    function! s:check_back_space() abort
      let col = col('.') - 1
      return !col || getline('.')[col - 1]  =~# '\s'
    endfunction
    ''

    # Use <c-space> to trigger completion.
    "inoremap <silent><expr> <c-space> coc#refresh()"

    # Use <cr> to confirm completion, `<C-g>u` means break undo chain at current position.
    # Coc only does snippet and additional edit on confirm.
    ''inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"''

    # Use `[c` and `]c` to navigate diagnostics
    (nmap {silent=true; plugin=true; key="[c"; cmd="coc-diagnostic-prev";})
    (nmap {silent=true; plugin=true; key="]c"; cmd="coc-diagnostic-next";})

    # Remap keys for gotos
    (nmap {silent=true; plugin=true; key="gd"; cmd="coc-definition";})
    (nmap {silent=true; plugin=true; key="gy"; cmd="coc-type-definition";})
    (nmap {silent=true; plugin=true; key="gi"; cmd="coc-implementation";})
    (nmap {silent=true; plugin=true; key="gr"; cmd="coc-references";})

    # Use K to show documentation in preview window
    ''
    nnoremap <silent> K :call <SID>show_documentation()<CR>

    function! s:show_documentation()
      if (index(['vim','help'], &filetype) >= 0)
        execute 'h '.expand('<cword>')
      else
        call CocAction('doHover')
      endif
    endfunction
    ''

    # Remap for rename current word
    (nmap {plugin=true; key="${leader}rn"; cmd="coc-rename";})

    # Remap for format selected region
    ''
    xmap ${leader}f  <Plug>(coc-format-selected)
    nmap ${leader}f  <Plug>(coc-format-selected)

    augroup mygroup
      autocmd!
      " Setup formatexpr specified filetype(s).
      autocmd FileType typescript,json setl formatexpr=CocAction('formatSelected')
      " Update signature help on jump placeholder
      autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
    augroup end
    ''

    # Remap for do codeAction of selected region, ex: `<leader>aap` for current paragraph
    ''
    xmap ${leader}a  <Plug>(coc-codeaction-selected)
    nmap ${leader}a  <Plug>(coc-codeaction-selected)
    ''

    # Remap for do codeAction of current line
    "nmap ${leader}ac  <Plug>(coc-codeaction)"
    # Fix autofix problem of current line
    "nmap ${leader}qf  <Plug>(coc-fix-current)"

    # Use <tab> for select selections ranges, needs server support, like: coc-tsserver, coc-python
    ''
    nmap <silent> <TAB> <Plug>(coc-range-select)
    xmap <silent> <TAB> <Plug>(coc-range-select)
    xmap <silent> <S-TAB> <Plug>(coc-range-select-backword)
    ''

    # Use `:Format` to format current buffer
    "command! -nargs=0 Format :call CocAction('format')"

    # Use `:Fold` to fold current buffer
    "command! -nargs=? Fold :call     CocAction('fold', <f-args>)"

    # use `:OR` for organize import of current buffer
    "command! -nargs=0 OR   :call     CocAction('runCommand', 'editor.action.organizeImport')"

    # Add status line support, for integration with other plugin, checkout `:h coc-status`
    "set statusline^=%{coc#status()}%{get(b:,'coc_current_function','''''')}"

    # Using CocList
    # Show all diagnostics
    "nnoremap <silent> <space>a  :<C-u>CocList diagnostics<cr>"
    # Manage extensions
    "nnoremap <silent> <space>e  :<C-u>CocList extensions<cr>"
    # Show commands
    "noremap <silent> <space>c  :<C-u>CocList commands<cr>"
    # Find symbol of current document
    "nnoremap <silent> <space>o  :<C-u>CocList outline<cr>"
    # Search workspace symbols
    "nnoremap <silent> <space>s  :<C-u>CocList -I symbols<cr>"
    # Do default action for next item.
    "nnoremap <silent> <space>j  :<C-u>CocNext<CR>"
    # Do default action for previous item.
    "nnoremap <silent> <space>k  :<C-u>CocPrev<CR>"
    # Resume latest coc list
    "nnoremap <silent> <space>p  :<C-u>CocListResume<CR>"

    # # https://github.com/neoclide/coc.nvim/wiki/Multiple-cursors-support
    # ''
    # nmap <silent> <C-c> <Plug>(coc-cursors-position)
    # nmap <silent> <C-n> <Plug>(coc-cursors-word)
    # xmap <silent> <C-n> <Plug>(coc-cursors-range)
    # " use normal command like `<leader>xi(`
    # nmap <leader>x  <Plug>(coc-cursors-operator)
    # ''

    # use out-of-the-box airline integration
    "let g:airline#extensions#coc#enabled = 1"
    # add status to the end of your statusline
    "set statusline^=%{coc#status()}"
  ];
}
