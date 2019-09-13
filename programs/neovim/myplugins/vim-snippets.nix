{ pkgs, ... }:
{
  plugins = [
    pkgs.vimPlugins.vim-snippets  # default snippets
  ];
  extraConfig = [];
}
