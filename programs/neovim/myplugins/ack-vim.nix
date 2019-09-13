{ pkgs, lib, pluginBuilder, ... }:
{
  description = "file search";
  pkg = pkgs.vimPlugins.ack-vim;
}

