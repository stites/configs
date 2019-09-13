{ pkgs, ... }:
{
  plugins = [
    pkgs.vimPlugins.limelight-vim           # highlight cursor's text object (paragraph) when in writing mode
  ];
  extraConfig = [];
}
