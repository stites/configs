{ pkgs, pluginBuilder, ... }:
{
  pkg = pluginBuilder rec {
   name = "vim-textobj-user";
   tarball = "${homepage}/archive/master.tar.gz";
   homepage = https://github.com/kana/vim-textobj-user;
  };
}
