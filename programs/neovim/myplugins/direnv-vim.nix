{ pkgs, ... }:
{
  plugins = [
    pkgs.vimPlugins.direnv-vim
  ];
  extraConfig = [];
}
