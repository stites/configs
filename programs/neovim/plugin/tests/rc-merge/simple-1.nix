{ pkgs, ... }:
{
  pkg = pkgs.vimPlugins.ultisnips;
  extraConfig = ''
    ./simple-1.nix
  '';
}
