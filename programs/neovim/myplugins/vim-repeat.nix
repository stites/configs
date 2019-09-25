{ pkgs, lib, ... }:
{
  description = "use . after a plugin map and have it work";
  pkg = pkgs.vimPlugins.vim-repeat;
  # example config:
  # extraConfig = ''
  #   silent! call repeat#set("\<Plug>MyWonderfulMap", v:count)
  # '';
}
