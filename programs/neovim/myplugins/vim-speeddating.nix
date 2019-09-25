{ pkgs, lib, ... }:
{
  description = "increment dates with <c-x> and <c-a>";
  pkg = pkgs.vimPlugins.vim-speeddating;
  dependencies = [
    ./vim-repeat.nix
  ];
}
