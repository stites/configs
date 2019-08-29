{ lib, pkgs, ... }:
{
  rc = lib.strings.concatStringsSep "\n" [ ];
  plugins = [
    pkgs.vimPlugins.vim-nix # syntax highlighting
  ];
}
