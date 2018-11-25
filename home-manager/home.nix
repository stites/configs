{ pkgs, lib, ... }:

let
  stdenv = pkgs.stdenv;
  homedir = builtins.getEnv "HOME";
  ca-bundle_crt = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt"; # just in case
  lib = stdenv.lib;
  exe = pkgs.haskell.lib.justStaticExecutables;
  neovim = import ./programs/neovim { inherit pkgs lib; };
  mail = import ./mail.nix;
in
{
  manual.html.enable = true;
  manual.manpages.enable = true;

  home = {
    sessionVariables = {
      # so that electron apps play nicely with taffybar
      XDG_CURRENT_DESKTOP = "Unity";
    };
    packages = (import ./packages.nix { inherit pkgs; })
      ++ [ pkgs.protonmail-bridge pkgs.screen ];
    keyboard = {
      layout = "us";
      variant = "colemak";
      options = [ "ctrl:nocaps" ];
    };

    file.".fasdrc".text = ''
      # Fasd defaults to track your "$PWD". Set this to 0 to disable this behavior.
      # _FASD_TRACK_PWD=0

      # List of blacklisted strings. Commands matching them will not be processed, defaults to "--help"
      # _FASD_BLACKLIST="--help"

      # List of all commands that needs to be shifted, defaults to "sudo busybox".
      # _FASD_SHIFT="sudo busybox"

      # List of all commands that will be ignored, defaults to "fasd ls echo".
      # _FASD_IGNORE="fasd ls echo"
    '';


    #file.".npmrc".text = ''
    #  init-author-name=Sam Stites
    #  init-author-email=sam@stites.io
    #  init-author-url=https://stites.io
    #  init-version=0.0.1
    #  python=python2.7
    #  progress=true
    #  parseable=true
    #  loglevel=warn
    #'';

    file.".aspell.en.pws".text = ''
      personal_ws-1.1 en 2 utf-8
      differentiable
      invariants
      PyTorch
      TensorFlow
      Keras
      dplyr
      Hasktorch
      stites
      compositionality
      treebank
      monoidal
      formalise
      amongst
      compositional
      backpropagation
    '';

    file.".ghci".source                            = ./haskell/configs/ghci;
    file.".haskline".source                        = ./haskell/configs/haskline;
    file.".stack/config.yaml".source               = ./haskell/configs/stack-local.yaml;
    file.".stack/global-project/stack.yaml".source = ./haskell/configs/stack-global.yaml;
  };

  nixpkgs.config = import ./config.nix;

  xdg = {
    enable = true;
    configFile = {
      "nixpkgs/config.nix".source = ./config.nix;
      "taffybar/taffybar.hs" = {
        source = ./xmonad/taffybar.hs;
        onChange = "rm -rf ${homedir}/.cache/taffybar/";
      };
      "taffybar/taffybar.css" = {
        source = ./xmonad/taffybar.css;
        onChange = "rm -rf ${homedir}/.cache/taffybar/";
      };
      "nvim/UltiSnips" = {
        recursive = true;
        source = ./programs/neovim/UltiSnips;
      };
      "urxvt/color-themes" = {
        recursive = true;
        source = pkgs.fetchFromGitHub {
          owner = "felixr";
          repo = "urxvt-color-themes";
          rev = "56589a340f76c26486d8575fa639834aa7c248ea";
          sha256 = "1jhk606ayd1qkphm63da7g5wy4y68n1bjdqjwrjy37nxsri01hvy";
        };
      };
    };
    dataFile = neovim.xdg.dataFile // {
      "home-manager-dev" = {
        executable = true;
          target = "../bin/home-manager-dev";
          text = ''
            #!/usr/bin/env bash
            home-manager -I home-manager=$HOME/git/home-manager $@
          '';
        };
    };
  };


  fonts.fontconfig.enableProfileFonts = true;
  xsession = {
    enable = true;
    preferStatusNotifierItems = true;

    pointerCursor = {
      size = 64;
      name = "Vanilla-DMZ";
      package = pkgs.vanilla-dmz;
    };

    windowManager.xmonad = {
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
  };

  gtk = {
    enable = true;
    gtk3.waylandSupport = true;
    font = {
      name = "DejaVu Sans 12";
      package = pkgs.dejavu_fonts;
    };
  };
  qt = {
    enable = true;
    useGtkTheme = true;
  };


  services.flameshot.enable = true;
  services.taffybar.enable = true;
  services.status-notifier-watcher.enable = true;
  services.syncthing = {
    enable = false;
    tray = true;
  };
  services.udiskie.enable = true;

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

  services.keybase.enable = true;
  services.kbfs.enable = true;

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;

  #services.gnome-keyring.enable = true;
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
  services.blueman-applet.enable = false;
  services.network-manager-applet.enable = true;
  services.pasystray.enable = true;
  services.parcellite.enable = true;
  services.compton.enable = true;
  # services.protonmail-bridge.enable = true;

  programs = {
    home-manager = {
      enable = true;
      path = https://github.com/rycee/home-manager/archive/master.tar.gz;
      # path = "${homedir}/git/home-manager";
    };

    bash = import ./programs/bash.nix { inherit pkgs lib; };

    autorandr.enable = true;
    command-not-found.enable = true;
    direnv.enable = true;
    direnv.enableBashIntegration = true;
    feh.enable = true;
    htop.enable = true;
    lesspipe.enable = true;
    man.enable = true;
    noti.enable = true;
    zathura.enable = true;

    texlive = {
      enable = false;
      package = pkgs.texlive; #.combined.scheme-full;
      # extraPackages = tpkgs: { inherit (tpkgs) collection-fontsrecommended algorithms; };
    };
    ssh    = import ./programs/ssh.nix;
    fzf    = import ./programs/fzf.nix;
    git    = import ./programs/git.nix;
    tmux   = import ./programs/tmux.nix { inherit pkgs; };
    urxvt  = import ./programs/urxvt.nix { inherit pkgs; };
    neovim = neovim.config;
  } // mail.programs;

  accounts.email = mail.email;

  systemd.user.services.protonmail-bridge = {
    Unit = {
      Description = "ProtonMail Bridge";
      # Requires = [ "network.target" ];
      # After    = [ "network.target" ];
    };

    Service = {
      # ExecStart ="${pkgs.protonmail-bridge}/bin/Desktop-Bridge --cli";
      ExecStart ="${pkgs.screen}/bin/screen -dm ${pkgs.protonmail-bridge}/bin/Desktop-Bridge --cli";
      ExecStop ="${pkgs.killall}/bin/killall Desktop-Bridge";
      Restart = "on-failure";
      RestartSec = "5s";
      RemainAfterExit = "true";
      Type="forking";
    };

    Install = {
      WantedBy = [ "default.target" ];
    };
  };
}

