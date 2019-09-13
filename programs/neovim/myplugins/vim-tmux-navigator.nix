{ pkgs, lib, pluginBuilder, ... }:
{
  description = "Allow pane movement to jump out of vim into tmux";
  pkg = pkgs.vimPlugins.vim-tmux-navigator;
  extraConfig = [
    # Manually create key mappings (to avoid rebinding C-\)
    # "let g:tmux_navigator_no_mappings = 1"

    ''
    "nnoremap <silent> <C-h> :TmuxNavigateLeft<cr>"
    "nnoremap <silent> <C-j> :TmuxNavigateDown<cr>"
    "nnoremap <silent> <C-k> :TmuxNavigateUp<cr>"
    "nnoremap <silent> <C-l> :TmuxNavigateRight<cr>"
    ''
  ];

}

