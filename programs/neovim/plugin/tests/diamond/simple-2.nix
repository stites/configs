{ pkgs, ... }:
{
  pkg = pkgs.vimPlugins.neomake; # syntax highlighting
  extraConfig = [];
  dependencies = [
    ./dep/single.nix
  ];
}
