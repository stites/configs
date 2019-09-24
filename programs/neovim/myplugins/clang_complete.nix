{ pkgs, ... }:
{
  pkg = pkgs.vimPlugins.clang_complete; # Plug 'Rip-Rip/clang_complete', { 'for': 'c' }
  extraConfig = [
    ## clang_complete
    #"let g:clang_library_path='${pkgs.llvmPackages_7.clang-unwrapped.lib}/lib/'"
  ];
}
