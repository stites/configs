{ config, pkgs, lib, ... }:
let
  secrets = import ../secrets.nix;
  host = import ../host.nix;
in

assert host.hasTouchpad != null;
assert host.hasWireless != null;
assert host.xrandrHeads != null;

{
  ##################################################################################
  # Sounds
  ##################################################################################
  sound.enable = true;
  hardware.pulseaudio.enable = true;
  hardware.pulseaudio.systemWide = false;

  ##################################################################################
  # Wireless cards
  ##################################################################################
  networking.wireless = lib.attrsets.optionalAttrs host.hasWireless {
    # this would allow us to do declarative wireless networking via wpa_supplicant
    enable = false;
    networks = secrets.networks;
  };

  ##################################################################################
  # Graphical services
  ##################################################################################
  # urxvt daemons for urxvtc connections (lower memory overhead)
  # services.urxvtd.enable = true;
  nixpkgs.config.dmenu.enableXft = true;

  # Enable the X11 windowing system.
  # See: https://github.com/NixOS/nixpkgs/blob/release-18.09/nixos/modules/services/x11/xserver.nix
  # Note that when nvidia is disabled, the HDMI port will not work since it is hardwired into the GPU
  services.xserver = {
    enable = true;
    autorun = false;
    useGlamor = true;
    libinput.enable = host.hasTouchpad;
    multitouch.enable = host.hasTouchpad;

    # for debugging -- configuration can be found at /etc/X11/xorg.conf.
    exportConfiguration = true;

    # Add xinerama to xserver
    modules = [ pkgs.xorg.libXinerama ];
    serverLayoutSection = ''
      Option "Xinerama" "0"
    '';
    xrandrHeads = host.xrandrHeads;
    videoDrivers = host.videoDrivers;

    # desktopManager.default = "none";
    # desktopManager.default = "xfce";
    desktopManager.xfce.enable = true;
    desktopManager.xterm.enable = false;

    # Managed by home-manager!
    # windowManager = {
    #   default = "xmonad";
    #   xmonad = {
    #     enable = true;
    #     extraPackages = hpkgs: [
    #       hpkgs.taffybar
    #       hpkgs.xmonad-contrib
    #       hpkgs.xmonad-extras
    #     ];
    #   };
    # };

    displayManager.slim.enable = true;
    displayManager.slim.defaultUser = "stites";
    displayManager.slim.autoLogin = true;

    # These are not being run because there is no display manager being set??? or home-manager is overriding this
    displayManager.sessionCommands = ''
      export GTK_DATA_PREFIX=${config.system.path}
      export GTK_PATH=${config.system.path}/lib/gtk-3.0:${config.system.path}/lib/gtk-2.0
      export XCURSOR_PATH=~/.icons:~/.nix-profile/share/iconts:/var/run/current-system/sw/share/iconts
      ${pkgs.xorg.xset}/bin/xset r rate 220 50
    '';
  };
}
