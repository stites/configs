{ pkgs, ... }:
{
  pkg = pkgs.vimPlugins.coc-tabnine;
  dependencies = [ ./default.nix ];
}
