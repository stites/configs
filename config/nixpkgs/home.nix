{ pkgs, ... }:

let
  homedir = builtins.getEnv "HOME";
  ca-bundle_crt = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt"; # just in case
  lib = pkgs.stdenv.lib;
  exe = pkgs.haskell.lib.justStaticExecutables;
in
{
  home = {
    packages = import ./packages.nix {inherit pkgs; };
  };

  # imports = [
  #   ./modules/tmux
  # ];

  programs = {
    home-manager = {
      enable = true;
      path = https://github.com/rycee/home-manager/archive/master.tar.gz;
    };

    git = import ./programs/git.nix;
    tmux = import ./programs/tmux.nix { inherit pkgs; };
  };


}

