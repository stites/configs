{ pkgs, lib, pluginBuilder, ... }:
{
  pkg = pluginBuilder rec {
    name = "oceanic-next";
    tarball = "${homepage}/archive/master.tar.gz";
    homepage = https://github.com/mhartington/oceanic-next;
  };
}

