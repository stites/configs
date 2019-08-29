{ lib, pkgs, ... }:
{
  plugins = [
    pkgs.vimPlugins.LanguageClient-neovim   # specifying the build is handled in nix
  ];
  rc = lib.strings.concatStringsSep "\n" [
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
  ];

}
