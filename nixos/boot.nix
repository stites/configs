{ pinnedKernelPackages }: { config, pkgs, ...}:

{
  # add i915 to be lazy-loaded by xserver
  boot.initrd.availableKernelModules = [ "xhci_pci" "ahci" "nvme" "usb_storage" "sd_mod" "sdhci_pci" "i915" ];
  boot.initrd.kernelModules = [
    "acpi" "fb"
    # from https://github.com/Church-/NixOS-Config/blob/master/boot.nix
    "it87" "coretemp"
  ];

  hardware.enableAllFirmware = true;
  hardware.cpu.intel.updateMicrocode = true;

  boot.kernelPackages = pinnedKernelPackages;
  boot.blacklistedKernelModules = ["nouveau" "mei_me"];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;

  boot.loader.timeout = 8;
  boot.loader.efi.efiSysMountPoint = "/boot";
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.grub.useOSProber = true;
  boot.loader.grub.configurationLimit = 30;

  systemd.services.acpid.enable = true;
}
