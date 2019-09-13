{ pkgs, ... }:
{
  plugins = [
    pkgs.vimPlugins.deoplete-nvim
  ];
  extraConfig = [];
}
