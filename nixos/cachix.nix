{ config, pkgs, ... }:

{
  nix.binaryCaches = [
      "https://cache.nixos.org/"
      "https://nix-linter.cachix.org"
  ];
  nix.binaryCachePublicKeys = [
      "nix-linter.cachix.org-1:BdTne5LEHQfIoJh4RsoVdgvqfObpyHO5L0SCjXFShlE="
  ];
}

