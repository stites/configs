{ pkgs, lib, ... }:
{
  description = "theme statusbar";
  pkg = pkgs.vimPlugins.vim-airline-themes;
  dependencies = [
    ./vim-devicons.nix
  ];
}

