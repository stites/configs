{ pkgs, ... }:
{
  pkg = pkgs.vimPlugins.neomake; # syntax highlighting
  description = "";
  dependencies = [
    ./single.nix
  ];
}
