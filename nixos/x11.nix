{ useNvidia }: { config, pkgs, ... }:

{
  # dbus for xmonad
  services.dbus.packages = [ pkgs.gnome3.dconf ];

  # Enable UPower, which is used by taffybar
  services.upower.enable = true;
  systemd.services.upower.enable = true;

  environment.pathsToLink = [
    # Needed for GTK themes
    "/share"
  ];

  # urxvt daemons for urxvtc connections (lower memory overhead)
  # services.urxvtd.enable = true;
  nixpkgs.config.dmenu.enableXft = true;

  fonts = {
    fontconfig.enable = true;
    enableFontDir = true;
    enableGhostscriptFonts = true;
    fonts = with pkgs; [
      fira
      fira-code
      fira-mono
      # font-droid
      inconsolata
      ubuntu_font_family
      nerdfonts
      libre-caslon
      libertinus
    ];
  };

  # Enable the X11 windowing system.
  # See: https://github.com/NixOS/nixpkgs/blob/release-18.09/nixos/modules/services/x11/xserver.nix
  # Note that when nvidia is disabled, the HDMI port will not work since it is hardwired into the GPU
  services.xserver = {
    enable = true;
    autorun = false;
    useGlamor = true;
    libinput.enable = true;
    multitouch.enable = true;
    # for debugging -- configuration can be found at /etc/X11/xorg.conf.
    exportConfiguration = true;

    # Add xinerama to xserver
    modules = [ pkgs.xorg.libXinerama ];
    serverLayoutSection = ''
      Option "Xinerama" "0"
    '';

     # grothendieck specific
    xrandrHeads = [
      { output = "eDP1"; primary = true; }
      { output = "HDMI-0";
        # taken from XOrg-nvidia-generated.conf
        monitorConfig = ''
          Option "DPMS"
        '';
      }
    ];

    layout = "us";
    xkbVariant = "colemak";
    xkbOptions = "ctrl:nocaps";

    # If we do NOT specify a driver, intel is chosen (I think) and nvidia is _not_ loaded.

    # This works as "nvidia, use HDMI-0" (doesn't matter if HDMI is plugged in or not)
    # videoDrivers = if useNvidia then [ "nvidia" ] else [ "mesa" ];

    # This is use eDP1 faithfully (doesn't matter if HDMI is plugged in or not)
    videoDrivers = if useNvidia then [ "nvidia" "intel" ] else [ "mesa" ];

    # UNTESTED: This might work as the above with multi-monitor support on intel???
    # videoDrivers = if useNvidia then [ "mesa" "nvidia" ] else [ "mesa" ];

    # UNTESTED: ??? DON'T FORGET TO CHANGE modesetting.enable = true in nvidia.nix!!!
    # videoDrivers = if useNvidia then [ "modesetting" "nvidia" ] else [ "mesa" ];

    desktopManager.default = "none";
    desktopManager.xterm.enable = false;

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

  # Enable the i3 WM and LightDM login manager.
  # services.xserver.displayManager.lightdm.enable = true;
  # services.xserver.windowManager.gnom.enable = true;
}

