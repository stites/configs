{ pkgs, lib, ... }:

let
  stdenv = pkgs.stdenv;
  homedir = builtins.getEnv "HOME";
  ca-bundle_crt = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt"; # just in case
  lib = stdenv.lib;
  exe = pkgs.haskell.lib.justStaticExecutables;
  neovim = import ./programs/neovim { inherit pkgs lib; };
in
{
  manual.html.enable = true;
  manual.manpages.enable = true;

  home = {
    packages = import ./packages.nix { inherit pkgs; };
    keyboard = {
      layout = "us";
      variant = "colemak";
      options = [ "ctrl:nocaps" ];
    };

    file.".ghci".text = ''
      :set prompt "\ESC[0;34mghci>\ESC[m "
      :def hoogle \s -> Prelude.return Prelude.$ (":! hoogle --count=15 \"" :: Prelude.String) Prelude.++ s Prelude.++ ("\"" :: Prelude.String)
      :set -cpp -DASSERTS -DDEBUG

      :set -Wno-name-shadowing
      :set -XOverloadedStrings -XScopedTypeVariables -XTupleSections -XFlexibleContexts -XDataKinds

      -- :set +s
      -- :set -XPartialTypeSignatures

      -- import qualified Data.Text   as T
      -- import qualified Data.Vector as V
      -- import qualified Data.HashSet as HS
      -- import qualified Data.HashMap.Strict as HM
      -- import Data.Monoid ((<>))
    '';

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

    file.".haskline".text = ''
      bellStyle: NoBell
      maxHistorySize: Nothing
      editMode: Vi
      completionType: MenuCompletion
      completionPaging: True
      completionPromptLimit: Just 200
      listCompletionImmediately: True
      historyDuplicates: IgnoreConsecutive
      -- IgnoreConsecutive | IgnoreAll
    '';

    file.".npmrc".text = ''
      init-author-name=Sam Stites
      init-author-email=sam@stites.io
      init-author-url=https://stites.io
      init-version=0.0.1
      python=python2.7
      progress=true
      parseable=true
      loglevel=warn
    '';

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
    file.".stack/config.yaml".text = ''
      templates:
        params:
          author-name: Sam Stites
          author-email: 'fnz@fgvgrf.vb (cipher:rot13)'
          copyright: 'Copyright: (c) 2018 Sam Stites'
          github-username: stites

      # Have stack cooperate with nix
      nix:
        enable: true
        # pure set to `true` means that environment variables won't be forwarded when when running stack build
        # at the global level we _do_ want these sorts of things
        pure: false
        # pacakges passed to the nix environment when running `stack build` or `stack exec`. stack will NOT see
        # anything that hasen't been passed in here.

        packages:
          - zlib
    '';
    file.".stack/global-project/stack.yaml".text = ''
        # This is the implicit global project's config file, which is only used when
        # 'stack' is run outside of a real project.  Settings here do _not_ act as
        # defaults for all projects.  To change stack's default settings, edit
        # '/usr/home/stites/.stack/config.yaml' instead.
        #
        # For more information about stack's configuration, see
        # http://docs.haskellstack.org/en/stable/yaml_configuration/
        #
        flags: {}
        extra-package-dbs: []
        packages: []
        resolver: lts-10.4
        #extra-lib-dirs:
        #- /usr/local/lib/gcc46
        extra-deps:
        # for super-user-spark
        #- iostring-0.0.0.0
        #- validity-0.3.3.0
        #- validity-path-0.1.0.1

        # for haddock
        - haddock-api-2.18.1
        - haddock-library-1.4.4

        # for c2hsc
        - logging-3.0.5
      '';
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
      "nvim/snippets" = {
        recursive = true;
        source = ./programs/neovim/snippets;
      };
    };
    dataFile = neovim.xdg.dataFile;
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
    enable = true;
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
  services.blueman-applet.enable = true;
  services.network-manager-applet.enable = true;
  services.pasystray.enable = true;
  services.parcellite.enable = true;
  services.compton.enable = true;

  programs = {
    home-manager = {
      enable = true;
      path = https://github.com/rycee/home-manager/archive/master.tar.gz;
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

    offlineimap = {
      enable = true;
    };

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
  };

  systemd.user.services.weechat-daemon = {
    Unit = { Description = "Weechat daemon as a service"; };
    Service = {
      ExecStart = "${pkgs.weechat}/bin/weechat --daemon";
      Restart = "on-failure";
    };
    Install = {
      WantedBy = [ "default.target" ];
    };
  };

}

