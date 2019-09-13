{ pkgs, ... }:
{
  plugins = [
    pkgs.vimPlugins.undotree
  ];
  extraConfig = [
    # Show undo tree
    "nmap <silent> <leader>u :MundoToggle<CR>"
  ];
}
