{ pkgs, lib, pluginBuilder, ... }:
{
  description = "";
  pkg = pkgs.vimPlugins.fzf-vim;
  dependencies = [
    ./fzfWrapper.nix
  ];
  extraConfig = [
    # FZF commands:
    ''
    nmap ; :Buffers<CR>
    nmap <C-p> :Files<CR>
    nmap <Leader>t :Files<CR>
    nmap <Leader>r :Tags<CR>

    nmap <Esc><Esc> :cclose<CR>
    nmap \x :cclose<CR>
    ''
  ];
}

