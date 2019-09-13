{ lib, pkgs, pluginBuilder, ... }:
{
  plugins = [
    (pluginBuilder rec {
      name = "vim-bbye";
      tarball = "${homepage}/archive/master.tar.gz";
      homepage = https://github.com/moll/vim-bbye;
    })
  ];
  extraConfig = [];
}
