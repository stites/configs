{ pinnedKernelPackages }: # { pkgs, ... }:

{
  # make nvidia available
  boot.extraModulePackages = [ pinnedKernelPackages.nvidia_x11 ];

  hardware.opengl = {
    driSupport32Bit = true;
    # extraPackages = with pkgs; [ vaapiIntel ];
    extraPackages   = [      pinnedKernelPackages.nvidia_x11.out ];
    # extraPackages32 = [ pkgs_i686.linuxPackages.nvidia_x11.out ];
  };

  # Set up Optimus for the graphics card.
  hardware.nvidia.optimus_prime = {
    enable = true;
    nvidiaBusId = "PCI:1:0:0";
    intelBusId  = "PCI:0:2:0";
  };

  # This fails to work, but check out https://github.com/NixOS/nixpkgs/issues/44284
  # services.xserver.videoDrivers = [ "modesetting" ];
  # hardware.nvidia.modesetting.enable = true;
  # ^^^^^^^^^^^^THIS IS WORKING AT THE MOMENT
  hardware.nvidia.modesetting.enable = false;
  # ^^^^^^^^^^^^THIS IS _also_ WORKING
}
