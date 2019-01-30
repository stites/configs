# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, nixpkgs, ... }:

let
  nixos-unstable-pinned = import (builtins.fetchTarball {
    # Descriptive name to make the store path easier to identify
    name = "nixos-unstable_nvidia-410-66_2018-11-03";
    url = https://github.com/nixos/nixpkgs/archive/bf084e0ed7a625b50b1b0f42b98358dfa23326ee.tar.gz;
    sha256 = "0w05cw9s2pa07vqy21ack7g7983ig67lhwkdn24bzah3z49c2d8k";
  }) { };

  linuxPackages_latest = nixos-unstable-pinned.linuxPackages_latest;

  nixos-unstable = import <nixos-unstable> { };

  # USING LATEST FOR NVIDIA
  pinnedKernelPackages = nixos-unstable-pinned.linuxPackages_latest;
  useNvidia = true;
in

{
  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.packageOverrides = pkgs: {
    unstable = nixos-unstable;
    linuxPackages_latest = nixos-unstable-pinned.linuxPackages_latest;
    nvidia_x11 = nixos-unstable-pinned.nvidia_x11;

    haskellPackages = pkgs.haskellPackages.override {
      overrides = hpkgsNew: old:
      let
          dontCheck = pkgs.haskell.lib.dontCheck;
      in
      rec {
          diagrams-contrib = dontCheck old.diagrams-contrib;
          flat             = dontCheck old.flat;
          graphviz         = dontCheck old.graphviz;
          Frames           = dontCheck old.Frames;
      };
    };
  };

  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      (import ./boot.nix { inherit pinnedKernelPackages; })
     "${builtins.fetchGit { url="https://github.com/rycee/home-manager"; ref="master"; }}/nixos"
      ./system76
      ./tinc.nix
      ./fan.nix
      ./i18n.nix
      ./wireless.nix
      (import ./x11.nix { inherit useNvidia; })
      ./users.nix
      (import ./networking.nix { inherit pinnedKernelPackages; })
      (import ./packages.nix   { inherit pinnedKernelPackages; })
      ./time.nix
      ./sound.nix
      ./security.nix
      ./cachix.nix
    ] ++ (if useNvidia then [(import ./nvidia.nix { inherit pinnedKernelPackages; })] else []);

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "18.09"; # Did you read the comment?

  nix = {
    binaryCaches = [
      "https://cache.nixos.org/"
      "https://hie-nix.cachix.org"
    ];
    binaryCachePublicKeys = [
      "hie-nix.cachix.org-1:EjBSHzF6VmDnzqlldGXbi0RM3HdjfTU3yDRi9Pd0jTY="
    ];
    # POTENTIALLY USEFUL, BUT DON'T DO IT BECAUSE I AM NOT READY
    # nix.nixPath = [
    #   "nixpkgs=/root/nixpkgs"
    #   "nixos-config=/etc/nixos/configuration.nix"
    # ];
  };
}
