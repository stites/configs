{ pkgs, ... }:
{
  plugins = [
    pkgs.vimPlugins.vim-indent-guides
  ];
  extraConfig = [];
}
