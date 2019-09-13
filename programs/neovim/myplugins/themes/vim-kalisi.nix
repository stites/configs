{ pkgs, lib, pluginBuilder, ... }:
{
  pkg = pluginBuilder rec {
    name = "vim-kalisi";
    tarball = "${homepage}/archive/master.tar.gz";
    homepage = https://github.com/freeo/vim-kalisi;
  };
}

