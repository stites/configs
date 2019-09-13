{ pkgs, ... }:
{
  pkg = pkgs.vimPlugins.vim-snippets; # syntax highlighting
  dependencies = [
    ./dep/single.nix
  ];
}
