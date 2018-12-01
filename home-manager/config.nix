# { config, ... }:
{ ... }:

{
  allowUnfree = true;
  packageOverrides = pkgs_: (with pkgs_;
  let
    # _unstable = import <nixpkgs-unstable> { config = config.nixpkgs.config; };
    # _stable = import <nixpkgs-18.09> { config = config.nixpkgs.config; };
    _unstable = import <nixpkgs-unstable> { config = pkgs_.config; };
    _stable = import <nixpkgs-18.09> { config = pkgs_.config; };
  in {
    stable = _stable;
    unstable = _unstable;
    # this seems to cause OOM errors.
    # neovim = _unstable.neovim;

    slack = callPackage /home/stites/git/configs/home-manager/slack.nix {};

    signal-desktop = callPackage /home/stites/git/configs/home-manager/signal-desktop.nix {spellcheckerLanguage = "en_US";};

    haskellEnv = buildEnv {
      name = "haskellEnv";
      paths = [ python36 neovim cabal-install ];
    };

    golangEnv = buildEnv {
      name = "golangEnv";
      paths = [ dep2nix go2nix go ];
    };
  });
}
