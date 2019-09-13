{ lib, pkgs, pluginBuilder, ... }:
{
  plugins = [
    (pluginBuilder rec {
      name = "terminalkeys-vim";
      tarball = "${homepage}/archive/master.tar.gz";
      homepage = https://github.com/nacitar/terminalkeys.vim;
    })
  ];
  extraConfig = [];
}
