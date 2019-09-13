{ pkgs, ... }:
{
  plugins = [
    pkgs.vimPlugins.vim-eunuch
  ];
  extraConfig = [];
}
