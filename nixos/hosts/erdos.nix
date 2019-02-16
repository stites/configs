{ pkgs, ... }:
let
  useNvidia = (import ../host.nix).useNvidia;
in

{
  nixpkgs.config.packageOverrides = pkgs: {
    unstable = import (pkgs.fetchFromGitHub {
      # Descriptive name to make the store path easier to identify
      owner = "NixOS";
      repo = "nixpkgs-channels";
      rev = "929cc78363e6878e044556bd291382eab37bcbed";
      sha256 = "1ghzjk6fq8f2aimp23p45awnfzlcqc20sf7p1xp98myis1sqniwb";
    }) { };
  };

  services.znc = {
    enable = false;
  };

  # protonmail requires this:
  services.gnome3.gnome-keyring.enable = true;

  imports = [
    ../common.nix
    ../common/tinc.nix
    ../common/packages/ghc.nix
    ../common/packages/servers.nix
  ];
  ###################################################################
  # Boot configs for erdos on zfs
  ###################################################################

  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  services.openssh.enable = true;
  boot.zfs.devNodes = "/dev"; # fixes some virtualmachine issues
  boot.zfs.forceImportRoot = false;
  boot.zfs.forceImportAll = false;
  boot.kernelParams = [
    "boot.shell_on_fail"
    "panic=30" "boot.panic_on_fail" # reboot the machine upon fatal boot issues
  ];

  ###################################################################
  # generated from justdoit
  ###################################################################
  boot.loader.grub.device = "/dev/sda";

  networking.hostId = "fb26cee3"; # required for zfs use
}

