{ pkgs, ... }:
{
  pkg = pkgs.vimPlugins.coc-yank;
  extraConfig = [
    "nnoremap <silent> <space>y  :<C-u>CocList -A --normal yank<cr>"
  ];
}
