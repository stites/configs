{ pkgs, ... }:
{
  plugin = pluginBuilder rec {
    name = "lessspace-vim";
    tarball = "${homepage}/archive/master.tar.gz";
    homepage = https://github.com/thirtythreeforty/lessspace.vim;
  };
  description = "Trim trailing whitespace on lines you edit (or visit in insert mode)";
  dependencies = [];
  extraConfig = [];
}
