{ pkgs, lib, ... }:
{
  description = "add statusbar";
  pkg = pkgs.vimPlugins.vim-airline;
  extraConfig = [
    # Use powerline fonts for airline
    ''
    if !exists('g:airline_symbols')
      let g:airline_symbols = {}
    endif

    let g:airline_powerline_fonts = 1
    let g:airline_symbols.space = "\ua0"
    ''


  ];
}

