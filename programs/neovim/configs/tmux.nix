{ lib, pkgs, ... }:

{
  rc = lib.strings.concatStringsSep "\n" [
    # Manually create key mappings (to avoid rebinding C-\)
    "let g:tmux_navigator_no_mappings = 1"

    "nnoremap <silent> <C-h> :TmuxNavigateLeft<cr>"
    "nnoremap <silent> <C-j> :TmuxNavigateDown<cr>"
    "nnoremap <silent> <C-k> :TmuxNavigateUp<cr>"
    "nnoremap <silent> <C-l> :TmuxNavigateRight<cr>"

    ''
    vmap <silent> <Leader>rs SendSelectionToTmux
    nmap <silent> <Leader>rs NormalModeSendToTmux
    nmap <silent> <Leader>rv SetTmuxVars
    ''
  ];

  plugins = with pkgs.vimPlugins; [
    vim-tmux-navigator  # Allow pane movement to jump out of vim into tmux
    tslime              # send commands from vim to a running tmux session
  ];
}
