{ pkgs, lib, pluginBuilder, ... }:
{
  description = "[dark] haskell favorite theme";
  pkg = pkgs.vimPlugins.wombat256-vim;
  extraConfig = [
    # Colors and Fonts {{{
    ''
    try
      colorscheme wombat256mod
      " https://sunaku.github.io/vim-256color-bce.html
      " perma: https://perma.cc/Q6GC-ZD4A
      set term=screen-256color
    catch
    endtry
    ''
  ];
}

