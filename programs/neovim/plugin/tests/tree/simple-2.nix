{ pkgs, ... }:
{
  pkg = pkgs.vimPlugins.neomake; # syntax highlighting
  extraConfig = [];
  dependencies = [
  ];
}
