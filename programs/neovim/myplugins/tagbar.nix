{ pkgs, lib, ... }:
{
  description = "http://vimawesome.com/plugin/tagbar";
  pkg = pkgs.vimPlugins.tagbar;
  extraConfig = "map <leader>tt :TagbarToggle<CR>";
}

