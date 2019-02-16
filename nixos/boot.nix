{ pinnedKernelPackages }: { config, pkgs, ... }:

{
  boot.kernelModules = [ "it87" "coretemp" ];

  # add i915 to be lazy-loaded by xserver
  boot.initrd.availableKernelModules = [ "xhci_pci" "ahci" "nvme" "usb_storage" "sd_mod" "sdhci_pci" "i915" "enum" ];
  boot.initrd.kernelModules = [ "acpi" "fb" ];

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
  boot.loader.grub.configurationLimit = 10;

  systemd.services.acpid.enable = true;
}
