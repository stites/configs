{ pkgs, ... }:
{
  pkg = pkgs.vimPlugins.coc-prettier;
  extraConfig = [
    "command! -nargs=0 Prettier :CocCommand prettier.formatFile"
  ];
}
