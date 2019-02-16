{ config, pkgs, ... }:

let
  nixos18_09 = import (builtins.fetchTarball https://github.com/NixOS/nixpkgs-channels/archive/nixos-18.09.tar.gz) { };
  moz_overlay = import (builtins.fetchTarball https://github.com/mozilla/nixpkgs-mozilla/archive/master.tar.gz);

  mypython36 = pkgs:
    pkgs.python36.override {
      # packageOverrides = (self: super: {
      #   bokeh = super.bokeh.overridePythonAttrs (oldAttrs: { checkPhase = "true"; });
      #   pandas = super.pandas.overridePythonAttrs (oldAttrs: { checkPhase = "true"; });
      # });
    };

  mypythonPkgs = args: [
    (args.python36.withPackages (ps: with ps; [
      ############################################################
      # data science, munging, and analysis
      ############################################################
      beautifulsoup4
      pandas.overridePythonAttrs (oldAttrs: { checkPhase = "true"; })
      scikitlearn
      numpy
      pymc3
      pyro-ppl
      vowpalwabbit
      ipython
      imageio
      matplotlib
      pycv
      h5py
      (if args.useCuda then pytorchWithCuda else pytorch)
      scipy
      torchvision
      pyarrow
      numba
      dask
      dask-glm
      dask-image
      dask-jobqueue
      dask-ml
      dask-xgboost
      xgboost
      tensorflow-tensorboard
      tensorflowWithCuda

      ############################################################
      # interacting with the web
      ############################################################
      bokeh.overridePythonAttrs (oldAttrs: { checkPhase = "true"; })
      requests

      ############################################################
      # clean your code
      ############################################################
      mccabe
      mypy
      nose
      pycodestyle
      pydocstyle

      pygments
      pytest-mypy
      python-language-server
      pyls-isort
      pyls-mypy
      pyflakes
      yapf
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

  cuda-shell = cudas: pkgs.stdenv.mkDerivation {
    name = "cuda-env-shell";
    buildInputs = with pkgs;
      [ git gitRepo gnupg autoconf curl
        procps gnumake utillinux m4 gperf unzip
        cudas.cudatoolkit cudas.cudnn linuxPackages.nvidia_x11
        libGLU_combined
        xorg.libXi xorg.libXmu freeglut
        xorg.libXext xorg.libX11 xorg.libXv xorg.libXrandr zlib 
        ncurses5 stdenv.cc binutils
      ];
    shellHook = ''
       export CUDA_PATH=${cudas.cudatoolkit}
       # export LD_LIBRARY_PATH=${pkgs.linuxPackages.nvidia_x11}/lib:${pkgs.ncurses5}/lib:${cudas.cudnn}/lib
       export EXTRA_LDFLAGS="-L/lib -L${pkgs.linuxPackages.nvidia_x11}/lib"
       export EXTRA_CCFLAGS="-I/usr/include"
    '';
  };
  cuda9-shell  = cuda-shell { cudatoolkit = pkgs.cudatoolkit_9_0;  cudnn = pkgs.cudnn_cudatoolkit_9_0; };
  cuda10-shell = cuda-shell { cudatoolkit = pkgs.cudatoolkit_10_0; cudnn = pkgs.cudnn_cudatoolkit_10_0; };
in
{
  allowUnfree = true;
  allowUnsupportedSystem = true;
  android_sdk.accept_license = true;

  packageOverrides = pkgs_: (with pkgs_; {
    stable = nixos18_09;
    rbnix = rbnix pkgs_;
    tmuxp = tmuxp pkgs_;
    slack = callPackage /home/stites/git/configs/home-manager/slack.nix {};
    signal-desktop-beta = callPackage /home/stites/git/configs/home-manager/signal-desktop-beta.nix {spellcheckerLanguage = "en_US";};
    inherit hies cuda9-shell cuda10-shell;
    cuda-shell = cuda9-shell;
    reMarkable-sdk = reMarkable-sdk;

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
      paths = mypythonPkgs { python36 = (mypython36 pkgs); useCuda = false;};
    };

    pythonEnvWithCuda = buildEnv {
      name = "pythonEnvWithCuda";
      paths = mypythonPkgs {
        useCuda = true;
        python36 = (mypython36 pkgs);
      };
    };
  });

  systemd.user.services.weechat = {
    environment.WEECHAT_HOME = "/var/lib/weechat";
    serviceConfig = {
      User = "stites";
      Group = "users";
      # RemainAfterExit = "yes";
    };
    script = "${pkgs.weechat}/bin/weechat-headless --colors";
    wantedBy = [ "multi-user.target" ];
    wants = [ "network.target" ];
  };

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
  };
}
