{ pkgs, lib, ... }:
let
  install-python = false;
in
{
  imports = [
    ./jupyter.nix
    ./ipython.nix
  ];
  home.packages = lib.optionals install-python [
      (let
        dontCheck = pypkg: pypkg.overridePythonAttrs (old: {
          doCheck = false; checkPhase = "true";
        });

        jupyter = import (builtins.fetchGit {
          url = https://github.com/tweag/jupyterWith;
          rev = "02b929122d49189a896c9fae044c12db21846f25";
        }) {};

        jupyterEnvironment =
          jupyter.jupyterlabWith { kernels = [
            (jupyter.kernels.iPythonWith {
              name = "python";
              packages = p: with p; [ numpy
                nose (dontCheck celery) mock flake8 pygments          # testing
                hypothesis pytest pytest-mypy             # testing
                numpy # compute storage
                beautifulsoup4 requests                         # web processing
                # pandas # numba (dontCheck pyarrow) h5py                       # compute storage
                ipython notebook ipywidgets jupyter             # notebooks
                matplotlib seaborn tensorflow-tensorboard       # visualization
                scikitlearn scipy                               # statistics and ML
                pillow (dontCheck imageio) # pycv (not in nixpkgs) # CV
                nltk gensim                                     # NLP
                # # pymc3            # <<< TOTALLY BROKEN           # PPLs
                # pytorch-world.pytorch # mypytorch mytorchvision probtorch pyro-ppl    # future
              ];
            })
            (jupyter.kernels.iHaskellWith {
              name = "haskell";
              packages = p: with p; [ hvega formatting ];
            })
          ]; };

        python36 = pkgs.python36.override {
          packageOverrides = pself: psuper: {
            matplotlib  = dontCheck (psuper.matplotlib.override { enableQt = true; });
            scipy = dontCheck psuper.scipy;
            beautifulsoup4 = dontCheck psuper.beautifulsoup4;
          };
          self = python36;
        };

        # mypython36 = python36.withPackages (ps: with ps; [
        #   # nose celery mock flake8 pygments     # testing
        #   hypothesis pytest pytest-mypy                             # testing
        #   numpy  # compute storage
        # ] ++ map dontCheck [
        #   beautifulsoup4 requests                         # web processing
        #   pandas numba pyarrow h5py                       # compute storage
        #   ipython notebook ipywidgets jupyter             # notebooks
        #   matplotlib seaborn tensorflow-tensorboard       # visualization
        #   scikitlearn scipy                               # statistics and ML
        #   pillow imageio # pycv (not in nixpkgs) # CV
        #   nltk gensim                                     # NLP
        #   # # pymc3            # <<< TOTALLY BROKEN           # PPLs
        #   # pytorch-world.pytorch # mypytorch mytorchvision probtorch pyro-ppl    # future
        # ]);
      in (pkgs.stdenv.mkDerivation {
        name = "my-python-executables";
        buildPhase = "";
        # buildInputs = [ mypython36 pkgs.makeWrapper ];
        buildInputs = [ pkgs.makeWrapper ];
        propagatedBuildInputs = [ jupyterEnvironment ];
        src = ./.;
        installPhase = let
          wrapPyBin = bin: new: flags:
            "makeWrapper ${jupyterEnvironment}/bin/${bin} $out/bin/${new} "
            + (if flags != "" then "--add-flags \"${flags}\"" else "");
          in lib.strings.concatStringsSep "\n" [
            "mkdir -p $out/bin"
            # "${wrapPyBin  "python"          "py"  ""}"
            # "${wrapPyBin "ipython"          "ipy" "--profile=default"}"
            # "${wrapPyBin "jupyter"          "jp"  ""}"
            # "${wrapPyBin "jupyter-notebook" "nb"  ""}"
            # "${wrapPyBin "tensorboard"      "tb"  ""}"
          ];
      }))
    ];


}
