{
  allowUnfree = true;
  # overlays = [ (self: super: {
  #   weechat = super.weechat.overrideAttrs (oldAttrs: {
  #     version = "devel";
  #     src = self.fetchurl {
  #       url = "https://weechat.org/files/src/weechat-devel.tar.bz2";
  #       sha256 = "102i38hra1g0hml684hsilr2psc1klp3gy206zbndbzm9y75yddq";
  #     };
  #   });
  # } ) ];

  packageOverrides = pkgs_: with pkgs_; {
    slack = import /home/stites/git/configs/home-manager/slack.nix {
      inherit (pkgs_)
        stdenv dpkg fetchurl makeWrapper
        alsaLib atk cairo cups curl dbus expat
        fontconfig freetype glib gnome2 gtk3 gdk_pixbuf
        libnotify nspr nss pango systemd xorg libappindicator;
      inherit (pkgs_.xorg) libxcb;
    };

    signal-desktop = import /home/stites/git/configs/home-manager/signal-desktop.nix {
      inherit (pkgs_)
      stdenv lib fetchurl dpkg wrapGAppsHook
      gnome2 gtk3 atk cairo pango gdk_pixbuf glib freetype fontconfig
      dbus nss nspr alsaLib
      cups expat udev libnotify xorg
      hunspellDicts;

      inherit (pkgs_.xorg) libX11 libXi libXcursor libXdamage libXrandr libXcomposite
      libXext libXfixes libXrender libXtst libXScrnSaver;

      spellcheckerLanguage = "en_US";
      libappindicator = libappindicator;
    };

    haskellEnv = buildEnv {
      name = "haskellEnv";
      paths = [ python36 neovim cabal-install ];
    };

    golangEnv = buildEnv {
      name = "golangEnv";
      paths = [ dep2nix go2nix go ];
    };
  };
}
