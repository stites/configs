{ lib, pkgs, pluginBuilder, ... }:
{
  plugins = [
    pkgs.vimPlugins.ultisnips     # snippet manager
  ];
  extraConfig = [];
}
