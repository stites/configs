{ pkgs, ... }:
{
  plugins = [
    pkgs.vimPlugins.vim-grammarous          # grammar checking
  ];
  extraConfig = [];
}
