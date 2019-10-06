{ pkgs, ... }:

let
  config = pkgs.config;

  src-stub = pkgs.fetchurl {
    # url = https://static.stites.io/stub.tar.gz;
    url = https://s3-us-west-1.amazonaws.com/static.stites.io/stub.tar.gz;
    sha256 = "146sjf4j9lkzjs0hyhhrn2hz0mx5qg4lr625aliillca33w85qvf";
  };

  clang7-shell = (with pkgs; stdenv.mkDerivation {
    name = "clang7-env-shell";
    src = src-stub;
    dontBuild = true;
    buildInputs = [
      # building and debugging
      llvm_7
      clang_7
      (clang-tools.override{ llvmPackages = llvmPackages_7; }) # extra lang tools
      llvmPackages_7.openmp
      llvmPackages_7.libclang
      valgrind

      # builders
      # ninja     # llvm builder
      binutils  # not sure about this one
      autoconf  # not sure about this one
      # cmake     # simple building
      gnumake   # simple building
      bazel     # for multi-language
      (haskell.lib.justStaticExecutables haskellPackages.shake) # haskell builder

      # testing
      lit

      # code analysis
      cpplint
      cppcheck
      clang-analyzer
      include-what-you-use
      uncrustify            # code formatter

      # Extra libraries
      # FIXME: do we need to compile with llvm or can we just use the .so files????
      # (stxxl.override{ stdenv = llvmPackages_7.stdenv; })     # working on large OOM datasets
      (poco.override  { stdenv = llvmPackages_7.stdenv; })    # general purpose framework with networking focus
      (dlib.override  { stdenv = llvmPackages_7.stdenv; })    # general purpose machine learning library
      (loki.override  { stdenv = llvmPackages_7.stdenv; })    # contains flexible implementations of common design patterns and idioms
      (eigen.override { stdenv = llvmPackages_7.stdenv; })    # linalg and solvers
      (fcppt.override { stdenv = llvmPackages_7.stdenv; })    # Fruendlich's C++ toolkit which aims to improve c++ with typing and functional programming
      (boost168.override { stdenv = llvmPackages_7.stdenv; }) # collection of extras
    ];
    shellHook = ''
       export LD_LIBRARY_PATH="${llvm_7}/lib:${clang_7}/lib:${llvmPackages_7.openmp}/lib:${llvmPackages_7.libclang}/lib"
       # export EXTRA_LDFLAGS="-L/lib"
       # export EXTRA_CCFLAGS="-I/usr/include"
    '';
  });
  clang-shell = clang7-shell;

  mkcuda-shell = cudas: (with pkgs; stdenv.mkDerivation {
    name = "cuda-env-shell";
    src = src-stub;
    dontBuild = true;
    buildInputs = [
      # procps gnumake utillinux m4 gperf unzip
      # cudas.cudatoolkit cudas.cudnn linuxPackages.nvidia_x11
      # libGLU_combined
      # xorg.libXi xorg.libXmu freeglut
      # xorg.libXext xorg.libX11 xorg.libXv xorg.libXrandr zlib
      # ncurses5 stdenv.cc binutils
      autoconf
      binutils
      cudatoolkit
      curl
      freeglut
      git
      gitRepo
      gnumake
      gnupg
      gperf
      libGLU_combined
      linuxPackages.nvidia_x11
      m4
      ncurses5
      procps
      stdenv.cc
      unzip
      utillinux
      xorg.libX11
      xorg.libXext
      xorg.libXi
      xorg.libXmu
      xorg.libXrandr
      xorg.libXv
      zlib
    ];
    shellHook = ''
       export CUDA_PATH=${cudas.cudatoolkit}
       # export LD_LIBRARY_PATH=${pkgs.linuxPackages.nvidia_x11}/lib:${pkgs.ncurses5}/lib:${cudas.cudnn}/lib
       export EXTRA_LDFLAGS="-L/lib -L${pkgs.linuxPackages.nvidia_x11}/lib"
       export EXTRA_CCFLAGS="-I/usr/include"
    '';
  });
  cuda9-shell  = mkcuda-shell { cudatoolkit = pkgs.cudatoolkit_9_0;  cudnn = pkgs.cudnn_cudatoolkit_9_0; };
  cuda10-shell = mkcuda-shell { cudatoolkit = pkgs.cudatoolkit_10_0; cudnn = pkgs.cudnn_cudatoolkit_10_0; };
  cuda-shell   = cuda10-shell;
in
{
  allowUnfree = true;
  allowBroken = true; # really just for unreal engine 4
  allowUnsupportedSystem = true;
  android_sdk.accept_license = true;

  packageOverrides = pkgs_: (with pkgs_; {
    stable = import <nixos> { inherit config; };
    nur = import (builtins.fetchTarball "https://github.com/nix-community/NUR/archive/master.tar.gz") { pkgs = pkgs_; };
    inherit cuda9-shell cuda10-shell cuda-shell;
    inherit clang7-shell clang-shell;
  });

  # THIS ACTUALLY GOES INTO CONFIGURATION.NIX
  nix = {
    binaryCaches = [
      "https://cache.nixos.org/"
      "https://all-hies.cachix.org"
    ];
    binaryCachePublicKeys = [
      "all-hies.cachix.org-1:JjrzAOEUsD9ZMt8fdFbzo3jNAyEWlPAwdVuHw4RD43k="
    ];
    trustedUsers = [ "root" "stites" ];
    nixPath = [
      # "nixpkgs=$HOME/git/nix/nixpkgs/"
      # # "nixos-18_09=$HOME/git/nix/nixpkgs"
      # "home-manager=$HOME/git/nix/home-manager/"
      "nixpkgs-overlays=$HOME/git/config/overlays/"
    ];
  };
}
