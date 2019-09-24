{ pkgs, ... }:
{
  pkg = pkgs.vimPlugins.deoplete-clang;
  extraConfig = [
    # deoplete-clang
    "let g:deoplete#sources#clang#libclang_path='${pkgs.llvmPackages_7.clang-unwrapped.lib}/lib/libclang.so'"
    "let g:deoplete#sources#clang#clang_header='${pkgs.llvmPackages_7.clang-unwrapped}/lib/clang/7.0.1/include'"
    # MUST EXIST
    # "let g:deoplete#sources#clang#clang_complete_database='/home/stites/.config/nvim/deoplete-clang/compile_comands.json'"
  ];
}
