{ pkgs, ... }:
{
  pkg = pkgs.vimPlugins.ultisnips;
  extraConfig = ''
    ./simple-2.nix
  '';
}
