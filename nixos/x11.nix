{ useIntel }: { config, pkgs, ... }:

let
  nvidiaConfigs =
    {
      services.xserver.videoDrivers = [ "nvidia" ];

      #Set up Optimus for allowing my graphics card.
      hardware.nvidia.optimus_prime = {
        enable = true;
        nvidiaBusId = "PCI:1:0:0";
        intelBusId = "PCI:0:2:0";
      };
    };

  intelConfigs =
    { # "intel" doesn't use intel graphics
      services.xserver.videoDrivers = [ "mesa" ];
      # not sure if this actually works
      services.xserver.deviceSection = ''
        Option "Backlight"  "intel_backlight"
      '';
    };
in
  (if useIntel then intelConfigs else nvidiaConfigs) //
{
  # Enable the X11 windowing system.
  services.xserver = {
    enable = true;
    autorun = false;
    useGlamor = true;
    libinput.enable = true;
    multitouch.enable = true;

    layout = "us";
    xkbVariant = "colemak";
    xkbOptions = "ctrl:nocaps";

    desktopManager.default = "none";
    desktopManager.xterm.enable = false;

    windowManager = {
      default = "xmonad";
      xmonad = {
        enable = true;
        extraPackages = hpkgs: [
          hpkgs.taffybar
          hpkgs.xmonad-contrib
          hpkgs.xmonad-extras
        ];
      };
    };

    # displayManager.lightdm.enable = true;
    displayManager.slim.enable = true;
    displayManager.slim.defaultUser = "stites";
    displayManager.slim.autoLogin = true;

    displayManager.sessionCommands = ''
      export GTK_DATA_PREFIX=${config.system.path}
      export GTK_PATH=${config.system.path}/lib/gtk-3.0:${config.system.path}/lib/gtk-2.0
      export XCURSOR_PATH=~/.icons:~/.nix-profile/share/iconts:/var/run/current-system/sw/share/iconts
      ${pkgs.xorg.xset}/bin/xset r rate 220 50
    '';
  };
}

