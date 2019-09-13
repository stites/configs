{ pkgs, ... }:
{
  name = "vim-nix";
  pkg = pkgs.vimPlugins.vim-nix; # syntax highlighting
  extraConfig = [];
}
