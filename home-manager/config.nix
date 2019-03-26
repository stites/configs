{ pkgs, ... }:

let
  config = pkgs.config;
  nixos18_09 = import (builtins.fetchTarball https://github.com/NixOS/nixpkgs-channels/archive/nixos-18.09.tar.gz) { };
  moz_overlay = import (builtins.fetchTarball https://github.com/mozilla/nixpkgs-mozilla/archive/master.tar.gz);

  mypython36 = pkgs:
    pkgs.python36.override {
      packageOverrides = (self: super: {
        ############## CONFIGURE PARAMETERS #########################
        pytorchWithCuda = super.pytorchWithCuda.override {
          cudatoolkit = pkgs.cudatoolkit_10_0;
          cudnn = pkgs.cudnn_cudatoolkit_10_0;
        };

        ################### STOP TESTS ###########################
        numpy        = super.numpy.overridePythonAttrs             (oldAttrs: { doCheck = false; checkPhase = "true"; });
        pandas       = super.pandas.overridePythonAttrs            (oldAttrs: { doCheck = false; checkPhase = "true"; });
        scikitlearn  = super.scikitlearn.overridePythonAttrs       (oldAttrs: { doCheck = false; checkPhase = "true"; });

        ################# TOTALLY BROKEN ########################
        pymc3        = super.pymc3.overridePythonAttrs             (oldAttrs: { doCheck = false; checkPhase = "true"; });
        pyls = super.python-language-server.override {
          autopep8 = super.autopep8;
          mccabe = super.mccabe;
          pycodestyle = super.pycodestyle;
          pydocstyle = super.pydocstyle;
          pyflakes = super.pyflakes;
          rope = super.rope;
          yapf = super.yapf;
        };


      });
    };

  pyls = (ps: with ps; python-language-server.override {
    autopep8 = autopep8;
    mccabe = mccabe;
    pycodestyle = pycodestyle;
    pydocstyle = pydocstyle;
    pyflakes = pyflakes;
    rope = rope;
    yapf = yapf;
  });

  mypythonPkgs = args: [
    (args.python3.withPackages (ps: with ps; [
      ############################################################
      # data science, munging, and analysis
      ############################################################
      numpy
      scipy
      pandas
      pyarrow
      scikitlearn
      # pymc3 # <<< TOTALLY BROKEN
      ipython
      jupyter
      imageio
      matplotlib
      # pycv # <<< NOT IN NIXPKGS
      (if args.useCuda then pytorchWithCuda else pytorch)
      # (if args.useCuda then tensorflowWithCuda else tensorflow)
      # pyro-ppl
      # torchvision
      h5py
      tensorflow-tensorboard

      ############################################################
      # interacting with the web
      ############################################################
      bokeh
      beautifulsoup4
      requests

      ############################################################
      # clean your code
      ############################################################
      flake8
      pygments
      pytest-mypy
      hypothesis

      #### LSP
      # (pyls ps)
      pyls
      mccabe
      mypy
      nose
      pycodestyle
      pydocstyle
      pyflakes
      yapf
      #### optionals:
      pyls-isort
      pyls-mypy
    ]))
  ];

  _nixpkgsRubySource = pkgs_: pkgs_.fetchFromGitHub {
      owner = "bobvanderlinden";
      repo = "nixpkgs-ruby";
      rev = "aaf2d46c7e166fd4cd52cc71720b72eef2486f18";
      sha256 = "10rbw0kmbgq3jc2gngxqkdb6x4dkrh4fyrfqn6bx864vd4cszh5z";
    };

  rbnix = pkgs_: import (_nixpkgsRubySource pkgs_) { inherit pkgs; };

  reMarkable-sdk = (with pkgs; stdenv.mkDerivation rec {
    name = "reMarkable-sdk-${version}";
    version = "2.1.3";
    srcs = [
      "/tmp"
      (fetchurl {
        url = "https://remarkable.engineering/deploy/sdk/poky-glibc-x86_64-meta-toolchain-qt5-cortexa9hf-neon-toolchain-${version}.sh";
        sha256 = "1pqhv7npcnm72alxwzjyz1z448l1xr9bblx111mizgksgi7fykxl";
      })
    ];
    meta = with stdenv.lib; {
      description = "the reMarkable toolchain";
    };
    buildInputs = [ xz coreutils ];
    unpackPhase = ''
      mkdir -p /tmp/$src
      sed "s/\/usr\/bin\/env/env/g" $src > /tmp/$src/binary
    '';
    unpackCmd = ''
      echo "do nothing"
    '';
    buildPhase = ''
      ls /tmp/$src/binary
      bash /tmp/$src/binary
    '';
  });

  #  # Make your own easy-to-access attributes for the versions you use:
  #  ruby_2_5_1 = rbnix.getVersion ["2" "5" "1"];
  #  ruby_2_4_4 = rbnix.getVersion ["2" "4" "4"];

  #  # ... or use it directly as buildInput in your derivation:
  #  example = stdenv.mkDerivation {
  #    name = "example";
  #    buildInputs = [ (rbnix.getVersion ["2" "5" "1"]) ];
  #    installPhase = ''
  #      ruby --version > $out
  #    '';
  #  };

  hies = (import
    (pkgs.fetchFromGitHub {
      owner="domenkozar";
      repo="hie-nix";
      rev="19f47e0bf2e2f1a793bf87d64bf8266062f422b1";
      sha256="1px146agwmsi0nznc1zd9zmhgjczz6zlb5yf21sp4mixzzbjsasq";
    }) {}).hies;

  tmuxp = pkgs_: let
      pp = pkgs_.python.pkgs;
    in
      pp.buildPythonApplication rec {
        pname = "tmuxp";
        version = "1.4.2";

        src = pp.fetchPypi {
          inherit pname version;
          sha256 = "087icp1n1qdf53f1314g5biz16sigrnpqr835xqlr6vj85imm2dm";
        };

        postPatch = ''
          sed -i 's/==.*$//' requirements/base.txt requirements/test.txt
        '';

        checkInputs = [
          pkgs_.pytest
          pkgs_.pytest-rerunfailures
        ];

        # No tests in archive
        doCheck = false;

        propagatedBuildInputs = [
          pp.click pp.colorama pp.kaptan pp.libtmux
        ];

        meta = with pkgs_.stdenv.lib; {
          description = "Manage tmux workspaces from JSON and YAML";
          homepage = http://tmuxp.readthedocs.io;
          license = licenses.bsd3;
          platforms = platforms.linux;
          maintainers = with maintainers; [ jgeerds ];
        };
      };

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
    # stdenv = pkgs_.clangStdenv; # pkgs_.llvmPackages_7.stdenv;
    stable = nixos18_09;
    # tmuxp = tmuxp pkgs_;
    # slack = callPackage /home/stites/git/configs/home-manager/slack.nix {};
    # signal-desktop-beta = callPackage /home/stites/git/configs/home-manager/signal-desktop-beta.nix {spellcheckerLanguage = "en_US";};
    inherit hies;
    # inherit cuda9-shell cuda10-shell cuda-shell;
    # inherit clang7-shell clang-shell;

    # stable.haskell.packages.ghc844 = nixos18_09.stable.haskell.packages.ghc844.extend (sel: sup: {
    #   mkDerivation = drv: sup.mkDerivation (drv // { doHaddock = false; }); # jailbreak = true; });
    # });
    ubiquiti = pkgs_.stdenv.mkDerivation {
          name = "dumb-cryptomator";
          builder = ./builder.sh;
          dpkg = pkgs_.dpkg;
          src = pkgs_.fetchurl {
            url = "https://www.ui.com/download/unifi/default/default/unifi-network-controller-51019-debianubuntu-linux-and-unifi-cloud-key";
            sha256 = "0z7xrg9r7hrnqqjmmvgx5hixayq5fczd0ay3nxs7bmzgzkn48q1n";
          };
        };
        # full-cryptomator = nixpkgs.buildFHSUserEnv {
        #   name = "full-cryptomator";
        #   targetPkgs = pkgs: [ dumb-cryptomator ];
        #   multiPkgs = pkgs: [ pkgs.dpkg ];
        #   runScript = "Cryptomator";
        # };
      # };

    haskellPackages =
      pkgs_.haskellPackages.override {
        overrides = hpkgsNew: old:
        let
          dontCheck = pkgs.haskell.lib.dontCheck;
        in
        rec {
          # test-cereal = dontCheck old.test-cereal;
          # cereal = dontCheck old.cereal;
          # xmonad = dontCheck old.xmonad;
          # taffybar = dontCheck old.taffybar;
          # xmonad-extras = dontCheck old.xmonad-extras;
          # xmonad-contrib = dontCheck old.xmonad-contrib;
        };
      };

    golangEnv = buildEnv {
      name = "golangEnv";
      paths = with pkgs; [ dep2nix go2nix go ];
    };

    pythonEnv = buildEnv {
      name = "pythonEnv";
      paths = mypythonPkgs {
        python3 = (mypython36 pkgs);
        python3Packages = python36Packages;
        useCuda = false;
      };
    };

    pythonEnvWithCuda = buildEnv {
      name = "pythonEnvWithCuda";
      paths = mypythonPkgs {
        useCuda = true;
        python3 = mypython36 pkgs;
        python3Packages = python36Packages;
      };
    };
  });

  # THIS ACTUALLY GOES INTO CONFIGURATION.NIX
  nix = {
    binaryCaches = [
      "https://cache.nixos.org/"
      "https://hie-nix.cachix.org"
    ];
    binaryCachePublicKeys = [
      "hie-nix.cachix.org-1:EjBSHzF6VmDnzqlldGXbi0RM3HdjfTU3yDRi9Pd0jTY="
    ];
    trustedUsers = [ "root" "stites" ];
    nixPath = [
      "nixpkgs=$HOME/git/nix/nixpkgs/"
      # "nixos-18_09=$HOME/git/nix/nixpkgs"
      "home-manager=$HOME/git/nix/home-manager/"
      "nixpkgs-overlays=$HOME/git/config/overlays/"
    ];
  };
}
