{ pkgs, ... }:
{
  pkg = pkgs.vimPlugins.coc-highlight;
  extraConfig = [
    # Highlight symbol under cursor on CursorHold
    "autocmd CursorHold * silent call CocActionAsync('highlight')"
  ];
  coc-settings = {
    # use coc-highlight
    "coc.preferences.colorSupport" = true;
  };
}
