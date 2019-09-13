{ lib, pkgs, pluginBuilder, ... }:
{
  plugins = [
    # for more: https://github.com/mhinz/vim-galore/blob/master/PLUGINS.md#c-and-c
    # # C++ development
    # customPlugins.chromatica-nvim
    # clang_complete # Plug 'Rip-Rip/clang_complete', { 'for': 'c' }
    pkgs.vimPlugins.deoplete-clang
    (pluginBuilder rec {
      name = "chromatica-nvim";
      tarball = "${homepage}/archive/master.tar.gz";
      homepage = https://github.com/arakashic/chromatica.nvim;
    })
    # [?????] c++ theme???
    (pluginBuilder {
      name = "yaflandia";
      tarball = "https://github.com/JBakamovic/yaflandia/archive/master.tar.gz";
      homepage = https://github.com/JBakamovic/yaflandia;
    })
  ];
  rc = lib.strings.concatStringsSep "\n" [
    ## Chromatica
    #"let g:chromatica#libclang_path='${pkgs.llvmPackages_7.clang-unwrapped.lib}/lib/libclang.so'"
    ## "let g:chromatica#global_args = ['-isystem${pkgs.llvmPackages_7.clang-unwrapped}/include']"
    ## "let g:chromatica#libclang_path='${pkgs.llvmPackages_7.libcxx}/lib'"
    #"let g:chromatica#global_args = ['-isystem${pkgs.llvmPackages_7.clang-unwrapped}/lib/clang/7.0.1/include']"
    #"let g:chromatica#enable_at_startup=1"
    #"let g:chromatica#responsive_mode=1"

    # deoplete-clang
    "let g:deoplete#sources#clang#libclang_path='${pkgs.llvmPackages_7.clang-unwrapped.lib}/lib/libclang.so'"
    "let g:deoplete#sources#clang#clang_header='${pkgs.llvmPackages_7.clang-unwrapped}/lib/clang/7.0.1/include'"
    # MUST EXIST
    # "let g:deoplete#sources#clang#clang_complete_database='/home/stites/.config/nvim/deoplete-clang/compile_comands.json'"
    ## clang_complete
    #"let g:clang_library_path='${pkgs.llvmPackages_7.clang-unwrapped.lib}/lib/'"
  ];
}

