{ pkgs, ... }:

let
  browser = [
    "firefox.desktop" # installed by home-manager, overlay turns this into devedition
    # "firefox-devedition.desktop" # installed by home-manager
    # "userapp-Firefox Developer Edition-4Z1P8Z.desktop" # installed by nixos-rebuild
  ];
  associations = {
    "application/pdf" = [ "zathura" ];
    "text/html" = browser;
    "x-scheme-handler/http" = browser;
    "x-scheme-handler/https" = browser;
    "x-scheme-handler/ftp" = browser;
    "x-scheme-handler/chrome" = browser;
    "x-scheme-handler/about" = browser;
    "x-scheme-handler/unknown" = browser;
    "application/x-extension-htm" = browser;
    "application/x-extension-html" = browser;
    "application/x-extension-shtml" = browser;
    "application/xhtml+xml" = browser;
    "application/x-extension-xhtml" = browser;
    "application/x-extension-xht" = browser;
  };
in
{
  xdg.mimeApps.enable = true;
  xdg.mimeApps.associations.added = associations;
  xdg.mimeApps.defaultApplications = associations;
}
