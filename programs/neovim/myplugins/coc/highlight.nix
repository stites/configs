{ pkgs, ... }:
{
  pkg = pkgs.vimPlugins.coc-highlight;
  extraConfig = [
    # Highlight symbol under cursor on CursorHold
    "autocmd CursorHold * silent call CocActionAsync('highlight')"
  ];
}
