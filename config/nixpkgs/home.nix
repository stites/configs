{ pkgs, lib, ... }:

let
  stdenv = pkgs.stdenv;
  homedir = builtins.getEnv "HOME";
  ca-bundle_crt = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt"; # just in case
  lib = stdenv.lib;
  exe = pkgs.haskell.lib.justStaticExecutables;
  # xmonad = pkgs.xmonad-with-packages.override {
  #   packages = self: [ self.xmonad-extras self.xmonad-contrib self.taffybar ];
  # };
in
{
  home = {
    packages = import ./packages.nix { inherit pkgs; };
    keyboard = {
      layout = "us";
      variant = "colemak";
      options = [ "ctrl:nocaps" ];
    };
  };

  xdg.enable = true;

  xdg.configFile.taffyBar = {
    source = ./xmonad/taffybar.hs;
    target = "taffybar/taffybar.hs";
  };

  xsession.enable = true;
  xsession.pointerCursor = {
    size = 64;
    name = "Vanilla-DMZ";
    package = pkgs.vanilla-dmz;
  };

  xsession.windowManager.xmonad = {
    enable = true;
    enableContribAndExtras = true;
    config = ./xmonad/xmonad.hs;
    extraPackages = hpkgs: [
      hpkgs.xmonad-contrib
      hpkgs.xmonad-extras
      hpkgs.monad-logger
      hpkgs.taffybar
    ];
  };

  gtk = {
    enable = true;
    gtk3.waylandSupport = true;
    font = {
      name = "DejaVu Sans 12";
      package = pkgs.dejavu_fonts;
    };
  };

  services.taffybar.enable = true;
  services.status-notifier-watcher.enable = true;
  services.redshift ={
    enable = true;
    tray = true;
    brightness.night = "0.8";
    provider = "manual";
    latitude = "42.3601202";
    longitude = "-71.1318836";
    temperature = {
      day = 5500;
      night = 4000;
    };
  };

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  services.gpg-agent = {
    enable = true;
    enableSshSupport  = true;
    enableExtraSocket = false;
    maxCacheTtl       = 60480000;
    defaultCacheTtl   = 60480000;
    extraConfig = ''
      allow-preset-passphrase
    '';
  };

  programs = {
    home-manager = {
      enable = true;
      path = https://github.com/rycee/home-manager/archive/master.tar.gz;
    };

    bash = import ./programs/bash.nix { inherit pkgs lib; };

    fzf = {
      enable = true;
      enableBashIntegration = true;
      defaultCommand = "rg --files --hidden";
      defaultOptions = [ "--bind='ctrl-o:execute(nvim {})+abort'" ];
      fileWidgetCommand="$FZF_DEFAULT_COMMAND";
    };

    git = import ./programs/git.nix;
    tmux = import ./programs/tmux.nix { inherit pkgs; };
    neovim = {
      enable = true;
      extraPython3Packages = (ps: with ps; [ python-language-server ]);
      viAlias = true;
      vimAlias = true;
    };
    urxvt = {
      enable = true;
      package = pkgs.rxvt_unicode-with-plugins;
      fonts = [ "xft:FuraCode Nerd Font Mono:pixelsize=20" ];
      scroll.bar.enable = false;
      extraConfig = {
        # termName must be screen otherwise vim colorschemes will break in subtle ways
        "termName" = "rxvt-256color";
        "reverseVideo" = "True";
        "boldFont" = "xft:FuraCode Nerd Font Mono:bold:pixelsize=20";
        # Enable Shift-PageUp/Down in screen
        "secondaryScroll" = "true";
        # letterSpace: -1
        "urlLauncher" = "firefox";
        "matcher.button" = "1";
        "scrollstyle" = "plain";
        "URxvt*fading" = "30";
        "saveLines" = "4055";
        # "iso14755" = "False"; # unicode?
        # "iso14755_52" = "False"; # unicode?

        "perl-ext-common" = "default,matcher,font-size,color-themes";
        "color-themes.state-file" = "~/.urxvt-theme";
        "color-themes.autosave" = "1";
        "bell-command" = ''notify-send "Beep, Beep"'';
        "urgentOnBell" = "true";

        "color-themes.themedir" =  "~/git/configs/tools/urxvt/urxvt-color-themes/";
        "color-themes.preprocessor" = "/home/stites/.nix-profile/bin/cpp";
      };

      keybindings = {
        "M-C-l" =  "perl:color-themes:load-state";
        "M-C-s" =  "perl:color-themes:save-state";
        "M-escape" = "perl:keyboard-select:activate";
        "C-Up" =     "font-size:increase";
        "C-Down" =   "font-size:decrease";
        "C-S-Up" =   "font-size:incglobal";
        "C-S-Down" = "font-size:decglobal";
        "C-equal" =  "font-size:reset";
        "C-slash" =  "font-size:show";

        "Shift-Control-Left" =   "perl:color-themes:prev";
        "Shift-Control-Right" =  "perl:color-themes:next";

        "Shift-Control-V" = "eval:paste_clipboard";
        "Shift-Control-C" = "eval:selection_to_clipboard";
        "Shift-Control-Up" = "perl:font-size:increase";
        "Shift-Control-Down" = "perl:font-size:decrease";

        "Control-Up" =    "\\033[1;5A";
        "Control-K" =     "\\033[1;5A";
        "Control-Down" =  "\\033[1;5B";
        "Control-J" =     "\\033[1;5B";
        "Control-Left" =  "\\033[1;5D";
        "Control-H" =     "\\033[1;5D";
        "Control-Right" = "\\033[1;5C";
        "Control-L" =     "\\033[1;5C";
      };
    };
  };


}

