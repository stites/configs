{ pkgs, lib, config, ... }:

# WARNING: THIS SHOULD NOT BE USED ON NON-NIXOS BUILDS ANYMORE

let
  stdenv = pkgs.stdenv;
  host = pkgs.callPackage ./hosts { };
  homedir = host.homedir;
  confroot = host.confroot;
  ca-bundle_crt = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt"; # just in case
  lib = stdenv.lib;
  concatStringsSep = lib.strings.concatStringsSep;
  secrets = import ./secrets.nix;
  hpkgs822 = pkgs.haskell.packages.ghc822.extend (sel: sup: {
    mkDerivation = drv: sup.mkDerivation (drv // { doHaddock = false; }); # jailbreak = true; });
  });
  optionalString = lib.optionalString;
  termcommand = "kitty -1";
in
{
  manual.html.enable = true;
  manual.manpages.enable = true;

  programs = {
    home-manager = {
      enable = true;
      # path = "${homedir}/git/sys/home-manager";
    };
    zathura.enable = true;
    feh.enable = true;
    # -----------------------------
    # OSX OUT
    # firefox = {
    #   enable = true;
    #   enableAdobeFlash = false;
    #   enableGoogleTalk = true;
    #   enableIcedTea = false;
    #   package = pkgs.firefox-unwrapped;
    # };
    # -----------------------------
    # Some programs need SUID wrappers, can be configured further or are
    # started in user sessions.
    # mtr.enable = true;
  };

  home = {
    sessionVariables = {
      # so that electron apps play nicely with taffybar
      XDG_CURRENT_DESKTOP = "Unity";
      # GDK_SCALE=2;
      # GDK_DPI_SCALE="0.5";
    };
    keyboard = {
      layout = "us";
      variant = "colemak";
      options = [ "ctrl:nocaps" ];
    };
  };

  xdg = {
    enable = true;
    configFile = {
      "nixpkgs/isNixOS".text = "${if host.is.NixOS then "true" else "false"}";
      "nixpkgs/config.nix".source = "${confroot}/config.nix";
      # "nixpkgs/local-nixpkgs".source      = "${homedir}/git/configs/nixpkgs";
      # "nixpkgs/signal-desktop-beta.nix".source = "${confroot}/programs/signal-desktop-beta.nix";
    };
  };
  nixpkgs.config = import ./config.nix { inherit pkgs lib config; };

  fonts.fontconfig.enable = true;

  qt = {
    enable = host.is.NixOS;
    platformTheme = "gtk";
  };

  dconf  = {
    enable = true;
    settings = { };
  };

  xresources.extraConfig = ''
    Xft.dpi: 96
    Xft.autohint: 0
    Xft.lcdfilter: lcddefault
    Xft.hintstyle: hintfull
    Xft.hinting: 1
    Xft.antialias: 1
    Xft.rgba: rgb
  '';

  home.file.".XCompose".source = (pkgs.fetchFromGitHub {
    owner = "kragen";
    repo = "xcompose";
    rev = "3fb0a8ce54087bddf3d266c7de59d5e524750a6a";
    sha256 = "01nv5psi4klw06hp2i9q9wjw05mj0cwibrsp78vhn2gwb0lac1vv";
  }) + "/dotXCompose";

  xdg.configFile."pycodestyle".text =
    let
      ignorelist = lib.strings.concatStringsSep "," [
        "E222" # ignore "space after operator" to get text to line up
      ];
    in
      ''
        [pycodestyle]
        count = False
        ignore = ${ignorelist}
        max-line-length = 120
        statistics = True
      '';

  # -----------------------------
  # OSX OUT
  # services.blueman-applet.enable = true; # requires system install
  # services.dunst.enable = false;          # notification daemon
  services.network-manager-applet.enable = true;
  # services.pasystray.enable = true;         # PulseAudio system tray
  services.parcellite.enable = true;        # clipboard daemon
  # services.compton.enable = true;           # needs configuration
  services.xembed-sni-proxy.enable = true;
  # services.flameshot.enable = true;
  services.xsuspender.enable = true; # <<< I think this is throttling the mouse

  services.udiskie = {
    enable = true;
    tray = "always";
  };

  imports = [
    ./packages.nix
    ./programs/mail.nix
    ./programs/aspell
    ./programs/stack
    ./programs/glirc
    ./programs/neovim
    ./programs/bash
    ./programs/redshift
    ./programs/broot.nix
    ./programs/termonad
    (import ./programs/xmonad { inherit termcommand;})
    (import ./programs/rofi.nix { inherit termcommand; })
    ./programs/tmux.nix
    ./programs/gtk.nix
    ./programs/git.nix
    ./programs/fzf.nix
    ./programs/urxvt.nix
    ./programs/autorandr.nix
    ./programs/xscreensaver.nix
    ./programs/jupyter/jupyter_notebook_config.nix
    ./programs/tmuxifier.nix
    ./programs/haskline.nix
    ./programs/ghci.nix
    ./programs/codex.nix
    ./programs/fasd.nix
    ./programs/keybase.nix
    ./programs/ssh.nix
    ./programs/bat.nix
    ./programs/matplotlib.nix
    ./programs/brittany.nix
    ./programs/texlive.nix
    ./programs/jq.nix
    ./programs/neofetch.nix
    # ./programs/xsession.nix
  ];
}

