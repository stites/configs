{ pkgs, ... }:
{
  name = "dep0";
  pkg = pkgs.vimPlugins.vim-snippets; # syntax highlighting
}
