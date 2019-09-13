{ pkgs, ... }:
{
  name = "vim-nix";
  description = "";
  pkg = pkgs.vimPlugins.vim-nix; # syntax highlighting
  extraConfig = [];
}
