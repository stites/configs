{ lib, pkgs, ... }:
{
  plugins = [];
  rc = lib.strings.concatStringsSep "\n" [
    ''
    abbr Lunix Linux
    abbr accross across
    abbr hte the
    abbr Probablistic Probabilistic
    ''
  ];
}
