{ pkgs, pluginBuilder, ... }:
{
  pkg = pluginBuilder {
    name = "chromatica-nvim";
    rev = "master";
    homepage = https://github.com/arakashic/chromatica.nvim;
  };
  extraConfig = [
    ## Chromatica
    #"let g:chromatica#libclang_path='${pkgs.llvmPackages_7.clang-unwrapped.lib}/lib/libclang.so'"
    ## "let g:chromatica#global_args = ['-isystem${pkgs.llvmPackages_7.clang-unwrapped}/include']"
    ## "let g:chromatica#libclang_path='${pkgs.llvmPackages_7.libcxx}/lib'"
    #"let g:chromatica#global_args = ['-isystem${pkgs.llvmPackages_7.clang-unwrapped}/lib/clang/7.0.1/include']"
    #"let g:chromatica#enable_at_startup=1"
    #"let g:chromatica#responsive_mode=1"
  ];
}
