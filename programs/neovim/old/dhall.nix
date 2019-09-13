{ lib, pkgs, ... }:
{
  rc = lib.strings.concatStringsSep "\n" [ ];
  plugins = [
    pkgs.vimPlugins.dhall-vim
  ];
}
