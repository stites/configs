{ config, pkgs, ... }:

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
}
