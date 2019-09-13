{ lib, pkgs, pluginBuilder, ... }:
{
  plugins = [
    (pluginBuilder rec {
      name = "vim-bufkill";
      tarball = "${homepage}/archive/master.tar.gz";
      homepage = https://github.com/qpkorr/vim-bufkill;
    })
  ];
  extraConfig = [];
}
