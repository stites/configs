{ pkgs, ... }:
let
  host = pkgs.callPackage ../hosts {};
  confroot = host.confroot;
in
{
  xsession = {
    enable = host.is.NixOS;
    preferStatusNotifierItems = true;
    # windowManager.command = "startxfce4";
    # windowManager.command = "my-xmonad";

    # pointerCursor = {
    #   size = 128;
    #   name = "redglass";
    #   # package = pkgs.vanilla-dmz;
    # };

    windowManager = {
      # command = "/run/current-system/sw/bin/xfce4-session";
      xmonad = {
        enable = true;
        enableContribAndExtras = true;
        config = "${host.homedir}/.xmonad/xmonad.hs";
        # config = "${confroot}/programs/xmonad/xmonad.hs";
        # haskellPackages = hpkgs822;
        # extraPackages = hpkgs: with hpkgs [
        #   taffybar
        # ];
      };
    };
  };

}
