{ pkgs, ... }:

{
  xsession = {
    enable = (pkgs.callPackage ../hosts.nix { }).isNixOS;
    preferStatusNotifierItems = true;
    # windowManager.command = "startxfce4";
    # windowManager.command = "my-xmonad";

    # pointerCursor = {
    #   size = 128;
    #   name = "redglass";
    #   # package = pkgs.vanilla-dmz;
    # };

    # windowManager = {
    #   # command = "/run/current-system/sw/bin/xfce4-session";
    #   xmonad = {
    #     enable = true;
    #     enableContribAndExtras = true;
    #     config = "${confroot}/programs/xmonad/xmonad.hs";
    #     # haskellPackages = hpkgs822;
    #     # extraPackages = hpkgs: with hpkgs [
    #     #   taffybar
    #     # ];
    #   };
    # };
  };

}
