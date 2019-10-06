self: super:
let
  pyt-world = super.fetchFromGitHub {
    owner = "stites";
    repo = "pytorch-world";
    rev = "unstable"; # bcf6b53c208ccb416601f2e40f04a655b44421be
    sha256 = "0rjzl4ipci4y7jgbhjsmh28l7wa607y34f01nbc7p710fjf2cm6l";
  };

  pyt-pkgs = import "${pyt-world}/pin/nixpkgs.nix" { inherit (super) config; };
  mkl = pyt-pkgs.mkl;
  magma = self.callPackage "${pyt-world}/deps/magma_250.nix" {
    cudatoolkit = pyt-pkgs.cudatoolkit_10_0; mklSupport = true;
  };

  mypython36 = pyt-pkgs.python36.override {
    packageOverrides = pself: psuper: let

      dontCheck = pp: pp.overridePythonAttrs (old: {
        doCheck = false; checkPhase = "true";
      });

      numpy = pyt-pkgs.python36Packages.numpy.override { blas = pyt-pkgs.mkl; };

      pyworld-defaults = {
        inherit magma numpy mkl;
        mklSupport = true;
        openMPISupport = false;
        cudaSupport = false;
        buildNamedTensor = true;
        buildBinaries = true;
      };

    in
      {
      inherit dontCheck numpy;
      matplotlib  = psuper.matplotlib.override { enableQt = true; };
      pytorch-world = {
        pytorch = pyt-pkgs.python36Packages.callPackage "${pyt-world}/pytorch" pyworld-defaults;
        pytorchWithCuda = pyt-pkgs.python36Packages.callPackage "${pyt-world}/pytorch" (pyworld-defaults
          // {
          cudaSupport = true;
          cudatoolkit = pyt-pkgs.cudatoolkit_10_0;
          cudnn = pyt-pkgs.cudnn_cudatoolkit_10_0;
          nccl = pyt-pkgs.nccl_cudatoolkit_10;
        });
      };
    };
    self = mypython36;
  };
in {
  inherit magma mkl;
  # python 3.7 shouldn't be touched because of other transitive dependencies
  python36 = mypython36;
}
