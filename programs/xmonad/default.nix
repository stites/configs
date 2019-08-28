{ termcommand # more of an annotation to indicate that this requires updating a string somewhere
}:
{ pkgs, ... }:
let
  homedir = builtins.getEnv "HOME";
in
{
  # home.file.".xmonad/xmonad.hs" = {
  #   # source = ./xmonad.hs;
  #   source = ./xmonad-with-taffybar.hs;
  #   onChange = ''
  #     echo "Recompiling xmonad"
  #     $DRY_RUN_CMD xmonad --recompile

  #     # Attempt to restart xmonad if X is running.
  #     if [[ -v DISPLAY ]] ; then
  #       echo "Restarting xmonad"
  #       $DRY_RUN_CMD xmonad --restart
  #     fi
  #   '';
  # };
  xdg.configFile = {
    "taffybar/taffybar.hs" = {
      source = ../taffybar/taffybar.hs;
      onChange = "rm -rf ${homedir}/.cache/taffybar/";
    };
    "taffybar/taffybar.css" = {
      source = ../taffybar/taffybar.css;
      onChange = "rm -rf ${homedir}/.cache/taffybar/";
    };
  };

  services.taffybar.enable = true;
  services.status-notifier-watcher.enable = true;

  # services.taffybar = {
  #   enable = host.isNixOS;
  #   package = hpkgs822.taffybar;
  # };

  # services.status-notifier-watcher = {
  #   enable = host.isNixOS;
  #   package = hpkgs822.status-notifier-item;
  # };

  xsession = {
    enable = (pkgs.callPackage ../../hosts.nix { }).isNixOS;
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
        config = ./xmonad-with-taffybar.hs;
        # config = "${confroot}/programs/xmonad/xmonad.hs";
        # haskellPackages = hpkgs822;
        # extraPackages = hpkgs: with hpkgs [
        #   taffybar
        # ];
      };
    };
  };
}
