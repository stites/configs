{ config, pkgs, lib, ... }:

let
  # nixos-unstable-pinned = import (builtins.fetchTarball {
  #   # Descriptive name to make the store path easier to identify
  #   name = "nixos-unstable_nvidia-410-66_2018-11-03";
  #   url = https://github.com/nixos/nixpkgs/archive/bf084e0ed7a625b50b1b0f42b98358dfa23326ee.tar.gz;
  #   sha256 = "0w05cw9s2pa07vqy21ack7g7983ig67lhwkdn24bzah3z49c2d8k";
  # }) { };

  # linuxPackages_latest = nixos-unstable-pinned.linuxPackages_latest;

  # nixos-unstable = import <nixos-unstable> { };

  nixos-unstable = import (pkgs.fetchFromGitHub {
    # Descriptive name to make the store path easier to identify
    owner = "NixOS";
    repo = "nixpkgs-channels";
    rev = "929cc78363e6878e044556bd291382eab37bcbed";
    sha256 = "1ghzjk6fq8f2aimp23p45awnfzlcqc20sf7p1xp98myis1sqniwb";
  }) { };


  # USING LATEST FOR NVIDIA
  # pinnedKernelPackages = nixos-unstable-pinned.linuxPackages_latest;
  latest = true;
  pinnedKernelPackages = (if latest then nixos-unstable.linuxPackages_latest else nixos-unstable.linuxPackages);
  useNvidia = (import ../host.nix).useNvidia;
in

{
  nixpkgs.config.packageOverrides = pkgs: {
    unstable = nixos-unstable;
    # linuxPackages_latest = nixos-unstable-pinned.linuxPackages_latest;
    nvidia_x11 = pinnedKernelPackages.nvidia_x11;

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

  imports = [
    ../common.nix
    ../common/tinc.nix
    ../common/laptops.nix
    ../common/workstations.nix
    ../common/packages/ghc.nix
    ../common/packages/servers.nix
    ../common/packages/workstations.nix
  ] ++ (if useNvidia then [(import ../common/nvidia.nix { inherit pinnedKernelPackages; })] else []);

  ###########################################################################################
  # System76 driver stuff
  ###########################################################################################
  # Imports the overlay
  nixpkgs.overlays = [
    (self: super: {
      linuxPackages_latest = nixos-unstable.linuxPackages_latest.extend(lpself: lpsuper: {
        system76-dkms = (lpself.callPackage ../system76-nixos/system76-dkms {}).stable;
      });
      linuxPackages = nixos-unstable.linuxPackages.extend(lpself: lpsuper: {
        system76-dkms = (lpself.callPackage ../system76-nixos/system76-dkms {}).stable;
      });
    })
  ];
  boot.extraModulePackages = [ (if latest then pkgs.linuxPackages_latest else pkgs.linuxPackages).system76-dkms ];

  # This is required for system76-driver, I believe. Can I just add this to the nix script?
  boot.kernelParams = [ "ec_sys.write_support=1" ];

  # This line I think is not needed. Depends on when I'm supposed to load this
  # boot.kernelModules = [ "system76" ];

  # Must be kept in sync with system76
  boot.kernelPackages = pinnedKernelPackages;

  ###########################################################################################
  # Booting modules
  ###########################################################################################
  boot.kernelModules = [ "it87" "coretemp" ];

  # add i915 to be lazy-loaded by xserver
  boot.initrd.availableKernelModules = [ "xhci_pci" "ahci" "nvme" "usb_storage" "sd_mod" "sdhci_pci" "i915" "enum" ];
  boot.initrd.kernelModules = [ "acpi" "fb" ];

  hardware.enableAllFirmware = true;
  hardware.cpu.intel.updateMicrocode = true;

  # don't load nouveau and drop intel management engine
  boot.blacklistedKernelModules = ["nouveau" "mei_me"];

  ###########################################################################################
  # Bootloader
  ###########################################################################################

  # Use systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;

  boot.loader.timeout = 8;
  boot.loader.efi.efiSysMountPoint = "/boot";
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.grub.useOSProber = true;
  boot.loader.grub.configurationLimit = 10;

  ###########################################################################################
  # Fan control:
  ###########################################################################################
  environment.systemPackages = [ pkgs.lm_sensors ];

  systemd.services.fancontrol = {
    description = "Start fancontrol, if configured";
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      Type = "simple";
      ExecStart = "${pkgs.lm_sensors}/sbin/fancontrol";
    };
  };

  environment.etc."fancontrol".text = let
    mintemp="20";
    maxtemp="70"; # critical zone is 100, 60 is... quite noisy
    cpu_avg_sensor = "temp7_input"; # seven is the ... average sensor?
  in ''
    # Configuration file generated by pwmconfig, changes will be lost
    INTERVAL=10
    DEVPATH=hwmon0=devices/platform/coretemp.0 hwmon4=devices/platform/system76
    DEVNAME=hwmon0=coretemp hwmon4=system76
    FCTEMPS=hwmon4/pwm1=hwmon0/${cpu_avg_sensor} hwmon4/pwm2=hwmon0/${cpu_avg_sensor}
    FCFANS=hwmon4/pwm1=hwmon4/fan1_input hwmon4/pwm2=hwmon4/fan2_input
    MINTEMP=hwmon4/pwm1=${mintemp} hwmon4/pwm2=${mintemp}
    MAXTEMP=hwmon4/pwm1=${maxtemp} hwmon4/pwm2=${maxtemp}
    MINSTART=hwmon4/pwm1=182 hwmon4/pwm2=220
    MINSTOP=hwmon4/pwm1=12 hwmon4/pwm2=10
  '';

  # OLD CONFIGURATION:
  # environment.etc."fancontrol".text = ''
  #   # Configuration file generated by pwmconfig, changes will be lost
  #   DEVPATH=hwmon0= hwmon4=devices/platform/system76
  #   DEVNAME=hwmon0=acpitz hwmon4=system76
  #   FCTEMPS=hwmon4/pwm2=hwmon0/temp1_input hwmon4/pwm1=hwmon0/temp1_input
  #   FCFANS=hwmon4/pwm2=hwmon4/fan2_input hwmon4/pwm1=hwmon4/fan1_input
  #   MINTEMP=hwmon4/pwm2=20 hwmon4/pwm1=20
  #   MAXTEMP=hwmon4/pwm2=60 hwmon4/pwm1=60
  #   MINSTART=hwmon4/pwm2=150 hwmon4/pwm1=150
  #   MINSTOP=hwmon4/pwm2=16 hwmon4/pwm1=100
  # '';
}
