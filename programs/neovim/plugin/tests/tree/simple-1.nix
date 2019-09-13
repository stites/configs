{ pkgs, ... }:
{
  pkg = pkgs.vimPlugins.vim-snippets; # syntax highlighting
  dependencies = [
    ../single.nix
  ];
}
