{ config, pkgs, ... }:

{
  nix.binaryCaches = [
      "https://cache.nixos.org/"
      "https://nix-linter.cachix.org"
      "https://hie-nix.cachix.org"
  ];
  nix.binaryCachePublicKeys = [
      "nix-linter.cachix.org-1:BdTne5LEHQfIoJh4RsoVdgvqfObpyHO5L0SCjXFShlE="
      "hie-nix.cachix.org-1:EjBSHzF6VmDnzqlldGXbi0RM3HdjfTU3yDRi9Pd0jTY="
  ];
}

