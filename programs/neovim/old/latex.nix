{ lib, pkgs, ... }:
{
  rc = lib.strings.concatStringsSep "\n" [
    ''
    let g:vimtex_compiler_progname = 'nvr'
    ''


  ];
  plugins = [
    pkgs.vimPlugins.vimtex            # Plug 'lervag/vimtex', { 'for': 'latex' }
  ];
}
