{ pkgs, ... }:
{
  pkg = pkgs.vimPlugins.ultisnips;
  extraConfig = ''
    ./default.nix
  '';
  dependencies = [
    ./simple-1.nix
    ./simple-2.nix
  ];
}
