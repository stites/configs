# { config, ... }:
{ ... }:

let
  mypython36 = pkgs:
      pkgs.python36.override {
        # packageOverrides = (self: super: {
        #   bokeh = super.bokeh.overridePythonAttrs (oldAttrs: { checkPhase = "true"; });
        #   pandas = super.pandas.overridePythonAttrs (oldAttrs: { checkPhase = "true"; });
        # });
    };
  mypythonPkgs = args: [
    (args.python36.withPackages (ps: with ps; [
      beautifulsoup4
      bokeh.overridePythonAttrs (oldAttrs: { checkPhase = "true"; })
      h5py
      ipython
      matplotlib
      mypy
      nose
      numpy
      pygments
      pandas.overridePythonAttrs (oldAttrs: { checkPhase = "true"; })
      (if args.useCuda then pytorchWithCuda else pytorch)
      requests
      scipy
      scikitlearn
      torchvision
    ]))
  ];
in
{
  allowUnfree = true;
  # THIS ACTUALLY GOES INTO CONFIGURATION.NIX
  # nix = {
  #   binaryCaches = [
  #     "https://cache.nixos.org/"
  #     "https://nix-linter.cachix.org"
  #   ];
  #   binaryCachePublicKeys = [
  #     "nix-linter.cachix.org-1:<redacted>"
  #   ];
  #   trustedUsers = [ "root" "stites" ];
  # };
  packageOverrides = pkgs_: (with pkgs_;
  let
    # _unstable = import <nixpkgs-unstable> { config = config.nixpkgs.config; };
    # _stable = import <nixpkgs-18.09> { config = config.nixpkgs.config; };
    _unstable = import <nixpkgs-unstable> { config = pkgs_.config; };
    _stable = import <nixpkgs-18.09> { config = pkgs_.config; };
    _nixpkgsRubySource = pkgs_.fetchFromGitHub {
      owner = "bobvanderlinden";
      repo = "nixpkgs-ruby";
      rev = "aaf2d46c7e166fd4cd52cc71720b72eef2486f18";
      sha256 = "10rbw0kmbgq3jc2gngxqkdb6x4dkrh4fyrfqn6bx864vd4cszh5z";
    };
  in {
    rbnix = import _nixpkgsRubySource { inherit pkgs; };
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

    stable = _stable;
    unstable = _unstable;
    # this seems to cause OOM errors.
    # neovim = _unstable.neovim;

    slack = callPackage /home/stites/git/configs/home-manager/slack.nix {};

    signal-desktop = callPackage /home/stites/git/configs/home-manager/signal-desktop.nix {spellcheckerLanguage = "en_US";};

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
      paths = mypythonPkgs { python36 = (mypython36 pkgs); useCuda = true;};
    }; 
  });
}
