{ pkgs, ... }:
{
  description = ''enter "writing mode"'';
  pkg = pkgs.vimPlugins.goyo-vim;
  extraConfig = [
    ''
    function! ProseMode()
      call goyo#execute(0, [])
      set spell noci nosi noai nolist noshowmode noshowcmd
      set complete+=s
      set bg=light
      " if !has('gui_running')
      "   let g:solarized_termcolors=256
      " endif
      " colors solarized
    endfunction

    command! ProseMode call ProseMode()
    nmap \p :ProseMode<CR>
    ''
  ];
}
