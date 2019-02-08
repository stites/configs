{ config, pkgs, ... }:

{
  # boot.kernelPackages = pkgs.linuxPackages_4_18;
  boot.kernelPackages = pkgs.linuxPackages_latest;

  # Imports the overlay
  nixpkgs.overlays = [
    (self: super: {
      # linuxPackages_4_18 = super.linuxPackages_4_18.extend(lpself: lpsuper: {
      linuxPackages_latest = super.linuxPackages_latest.extend(lpself: lpsuper: {
        system76 = lpself.callPackage ./system76.nix {};
      });
    })
  ];

  boot.extraModulePackages = [ pkgs.linuxPackages_latest.system76 ];
  # boot.extraModulePackages = [ pkgs.linuxPackages_4_18.system76 ];

  # This line I think is not needed
  # boot.kernelModules = [ "system76" ];
}
