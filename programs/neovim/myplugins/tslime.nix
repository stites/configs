{ pkgs, lib, ... }:
{
  description = "send commands from vim to a running tmux session";
  pkg = pkgs.vimPlugins.tslime;
  extraConfig = ''
    vmap <silent> <leader>rs SendSelectionToTmux
    nmap <silent> <leader>rs NormalModeSendToTmux
    nmap <silent> <leader>rv SetTmuxVars
  '';
}
