{ pkgs, ... }:
{
  pkg = pkgs.vimPlugins.ultisnips; # syntax highlighting
  dependencies = [
    ./simple-1.nix
    ./simple-2.nix
  ];
}
