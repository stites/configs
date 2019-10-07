{ termcommand # more of an annotation to indicate that this requires updating a string somewhere
}:
{ pkgs, config, ... }:
let
  homedir = builtins.getEnv "HOME";
in
{
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

  xsession = {
    enable = true;
    preferStatusNotifierItems = true;
    # initExtra = ''
    #   export GTK_DATA_PREFIX=${config.system.path}
    #   export GTK_PATH=${config.system.path}/lib/gtk-3.0:${config.system.path}/lib/gtk-2.0
    #   # export XCURSOR_PATH=~/.icons:~/.nix-profile/share/iconts:/var/run/current-system/sw/share/iconts
    #   ${pkgs.xorg.xset}/bin/xset r rate 220 50
    # '';
    # profileExtra = ''
    #   eval $(${pkgs.gnome3.gnome-keyring}/bin/gnome-keyring-daemon --start -d --components=pksc11,secrets,ssh)
    #   export SSH_AUTH_SOCK
    # '';
    # windowManager.command = "startxfce4";
    # windowManager.command = "my-xmonad";

    # pointerCursor = {
    #   size = 128;
    #   name = "redglass";
    #   # package = pkgs.vanilla-dmz;
    # };

    windowManager = {
      xmonad = {
        enable = true;
        enableContribAndExtras = true;
        config = ./xmonad-with-taffybar.hs;
        extraPackages = hpkgs: with hpkgs; [
          taffybar
        ];
      };
    };
  };
}
