{ pkgs, ... }:

{
  nixpkgs.config.firefox = {
    enableGoogleTalkPlugin = true;
    enabledAdobeFlash = true;
    enabledAdobeFlashDRM = true;
    jre = false;
    icedtea = true;
  };

  # Enable CUPS to print documents.
  services.printing = {
    enable = true;
    browsing = true;
    defaultShared = true;
    tempDir = "/tmp";
    webInterface = true;
    drivers = [
      pkgs.gutenprint # drivers for most printers (use gutenprintBin for binary-only drivers)
      # pkgs.hplip # HP printers
      pkgs.hplipWithPlugin # HP printers with proprietary plugin run `nix run nixpkgs.hplipWithPlugin -c sudo hp-setup` to add the printer.
      # pkgs.splix # printers supporting SPL (Samsung Printer Language)
      # pkgsc.brlaser # Brother printers
    ];
  };

  # Use Avahi to find network printers as well.
  services.avahi = {
    enable = true; # please harden this configuration
    nssmdns = true;
  };

  #Turn on docker
  virtualisation.docker.enable = true;

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  services.postgresql = {
    enable = true;
    package = pkgs.postgresql100;
    # package = pkgs.postgresql_10;
    enableTCPIP = true;
    authentication = pkgs.lib.mkOverride 10 ''
      # Generated file; do not edit!
      # TYPE   DATABASE  USER    ADDRESS        METHOD
      local    all       all                    trust
      host     all       all     127.0.0.1/32   trust
      host     all       all     ::1/128        trust
    '';
  };

  # List packages installed in system profile. To search, run:
  environment.systemPackages = with pkgs; [
    #################################################################################
    # Workstations
    #################################################################################
    # mount usbs???
    ntfs3g usbutils

    # optimize power
    powertop xorg.xbacklight

    htop git micro rofi salt
    unstable.gitAndTools.git-annex
    unstable.gitAndTools.git-annex-metadata-gui

    # gtk libraries
    cairo
    cairo.out
    cairo.dev
    glib
    glib.out
    glib.dev
    gnome3.gtk3
    gnome3.gtk3.dev
    gnome3.gtksourceview
    gnome3.gtksourceviewmm
    gnome3.webkitgtk
    # gnome3.gobjectIntrospection
    # gnome3.gobjectIntrospection.dev

    # get required system-level GUI apps
    dmenu albert rxvt_unicode-with-plugins networkmanagerapplet
    networkmanager_dmenu

    # web-based GUI apps
    chromium firefox # slack signal-desktop
    zoom-us atom

    # multimonitor utils
    arandr autorandr

    # make sure you can access the filesystem
    xfce.thunar

    # in case you forget something on that other operating system you are dual booting
    # lvm2 cryptsetup
  ];
}
