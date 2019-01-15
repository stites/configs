{ pkgs, lib, config, ... }:

let
  stdenv = pkgs.stdenv;
  homedir = builtins.getEnv "HOME";
  ca-bundle_crt = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt"; # just in case
  lib = stdenv.lib;
  concatStringsSep = lib.strings.concatStringsSep;
  neovim = import ./programs/neovim { inherit pkgs lib; };
  mail = import ./mail.nix;
  host = import ./hosts.nix { inherit pkgs lib config; };
  hpkgs844 = pkgs.stable.haskell.packages.ghc844;
in
{
  manual.html.enable = true;
  manual.manpages.enable = true;

  home = {
    sessionVariables = {
      # so that electron apps play nicely with taffybar
      XDG_CURRENT_DESKTOP = "Unity";
      GDK_SCALE=2;
      GDK_DPI_SCALE="0.5";
    };
    packages = (import ./packages.nix { inherit pkgs lib config; })
      ++ [ pkgs.protonmail-bridge pkgs.screen ];
    keyboard = {
      layout = "us";
      variant = "colemak";
      options = [ "ctrl:nocaps" ];
    };

    file.".mailcap".text = ''
      text/html;  w3m -dump -o document_charset=%{charset} '%s'; nametemplate=%s.html; copiousoutput
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

    file.".codex".text = ''
      currentProjectIncluded: true
      hackagePath: ${homedir}/.cabal/packages/hackage.haskell.org/
      tagsFileHeader: false
      tagsFileName: codex.tags
      tagsFileSorted: false
      tagsCmd: hasktags --etags --follow-symlinks --output="$TAGS" "$SOURCES"
      stackOpts: ""
    '';

    file.".aspell.en.pws".source = "${homedir}/git/configs/home-manager/my-aspell-ws";
    file.".aspell.en.prepl".source = "${homedir}/git/configs/home-manager/my-aspell-repl";
    file.".ghci".source                            = ./haskell/configs/ghci;
    file.".haskline".source                        = ./haskell/configs/haskline;
    file.".stack/config.yaml".source               = ./haskell/configs/stack-local.yaml;
    file.".stack/global-project/stack.yaml".source = ./haskell/configs/stack-global.yaml;
    file.".mozilla/chrome/userChrome.css".text = ''
      /* Hide tab bar in FF Quantum */
      @-moz-document url("chrome://browser/content/browser.xul") {
        #TabsToolbar {
          visibility: collapse !important;
          margin-bottom: 21px !important;
        }

        #sidebar-box[sidebarcommand="treestyletab_piro_sakura_ne_jp-sidebar-action"] #sidebar-header {
          visibility: collapse !important;
        }
      }
    '';
  };

  nixpkgs.config = (import ./config.nix { inherit pkgs lib config; });
  nixpkgs.overlays = [];

  xdg = {
    enable = true;
    configFile = {
      "nixpkgs/isNixOS".text = "${if host.isNixOS then "true" else "false"}";
      "nixpkgs/config.nix".source = "${homedir}/git/configs/home-manager/config.nix";
      "nixpkgs/overlays" = {
        recursive = true;
        source = ./overlays;
      };
      "nixpkgs/local-nixpkgs".source      = "${homedir}/git/configs/nixpkgs";
      "nixpkgs/slack.nix".source          = "${homedir}/git/configs/home-manager/slack.nix";
      "nixpkgs/signal-desktop.nix".source = "${homedir}/git/configs/home-manager/signal-desktop-beta.nix";
      "taffybar/taffybar.hs" = {
        source = ./xmonad/taffybar.hs;
        onChange = "rm -rf ${homedir}/.cache/taffybar/";
      };
      "taffybar/taffybar.css" = {
        source = ./xmonad/taffybar.css;
        onChange = "rm -rf ${homedir}/.cache/taffybar/";
      };
      "nvim/UltiSnips/python.snippets" = {
        source = ./programs/neovim/UltiSnips/python.snippets;
      };

      "brittany/config.yaml".text = ''
        conf_debug:
          dconf_roundtrip_exactprint_only: false
          dconf_dump_bridoc_simpl_par: false
          dconf_dump_ast_unknown: false
          dconf_dump_bridoc_simpl_floating: false
          dconf_dump_config: false
          dconf_dump_bridoc_raw: false
          dconf_dump_bridoc_final: false
          dconf_dump_bridoc_simpl_alt: false
          dconf_dump_bridoc_simpl_indent: false
          dconf_dump_annotations: false
          dconf_dump_bridoc_simpl_columns: false
          dconf_dump_ast_full: false
        conf_forward:
          options_ghc: []
        conf_errorHandling:
          econf_ExactPrintFallback: ExactPrintFallbackModeInline
          econf_Werror: false
          econf_omit_output_valid_check: false
          econf_produceOutputOnErrors: false
        conf_preprocessor:
          ppconf_CPPMode: CPPModeAbort
          ppconf_hackAroundIncludes: false
        conf_obfuscate: false
        conf_roundtrip_exactprint_only: false
        conf_version: 1
        conf_layout:
          lconfig_reformatModulePreamble: true
          lconfig_altChooser:
            tag: AltChooserBoundedSearch
            contents: 3
          lconfig_allowSingleLineExportList: false
          lconfig_importColumn: 50
          lconfig_hangingTypeSignature: false
          lconfig_importAsColumn: 50
          lconfig_alignmentLimit: 30
          lconfig_indentListSpecial: true
          lconfig_indentAmount: 2
          lconfig_alignmentBreakOnMultiline: true
          lconfig_cols: 80
          lconfig_indentPolicy: IndentPolicyLeft
          lconfig_indentWhereSpecial: true
          lconfig_columnAlignMode:
            tag: ColumnAlignModeMajority
            contents: 0.7
      '';

      "nvim/UltiSnips/haskell.snippets" = {
        text = concatStringsSep "\n" [
          (builtins.readFile ./programs/neovim/UltiSnips/haskell.snippets)
          ''
            snippet box "" !b
            -------------------------------------------------------------------------------
            -- |
            -- Module    :  `!v HaskellModuleName()`
            -- Copyright :  (c) Sam Stites 2017
            -- License   :  BSD-3-Clause
            -- Maintainer:  ${(import ./secrets.nix).piis.address-rot13}
            -- Stability :  experimental
            -- Portability: non-portable
            -------------------------------------------------------------------------------

            endsnippet
          ''
        ];
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
          home-manager -I home-manager=${homedir}/git/home-manager $@
        '';
      };
      "remarkable-upload" = {
        executable = true;
        target = "../bin/remarkable-upload";
          text = ''
            #!/usr/bin/env bash
            f="$1"
            f=${ lib.concatStrings ["$" "{f//" "\\" "\\" "/\\" "\\" "\\" "\\" "}" ] }
            f=${ lib.concatStrings ["$" "{f//" "\\" ''"'' "/" "\\" "\\" "\\" ''"'' "}" ] }
            f=${ lib.concatStrings ["$" "{f//;/\\\\;}" ] }
            ${pkgs.curl}/bin/curl -w '\n' --form "file=@\"$f\"" "http://10.11.99.1/upload"
          '';
        };

    };
  };


  fonts.fontconfig.enableProfileFonts = true;
  xsession = {
    enable = host.isNixOS;
    preferStatusNotifierItems = true;

    pointerCursor = {
      size = 64;
      name = "Vanilla-DMZ";
      package = pkgs.vanilla-dmz;
    };

    windowManager.xmonad = {
      enable = host.isNixOS;
      enableContribAndExtras = host.isNixOS;
      config = ./xmonad/xmonad.hs;
      haskellPackages = hpkgs844;
      extraPackages = hpkgs: [
        hpkgs.xmonad-contrib
        hpkgs.xmonad-extras
        hpkgs.monad-logger
        hpkgs.taffybar
      ];
    };
  };

  gtk = {
    enable = host.isNixOS;
    gtk3 = {
      extraConfig = {
        scaling-factor = 2;
        gtk-cursor-blink = false;
        gtk-recent-files-limit = 20;
      };
    };
    theme = {
      package = pkgs.gnome3.gnome_themes_standard;
      # name = "Arc-Dark";
      name = "Adwaita";
    };
    font = {
      name = "DejaVu Sans 12";
      package = pkgs.dejavu_fonts;
    };
  };
  qt = {
    enable = host.isNixOS;
    useGtkTheme = true;
  };

  services.flameshot.enable = true;
  services.taffybar = {
    enable = host.isNixOS;
    package = hpkgs844.taffybar;
  };

  services.status-notifier-watcher = {
    enable = host.isNixOS;
    package = hpkgs844.status-notifier-item;
  };

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
      # path = https://github.com/rycee/home-manager/archive/release-18.09.tar.gz;
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

    firefox = {
      enable = true;
      enableAdobeFlash = false;
      enableGoogleTalk = true;
      enableIcedTea = false;
      package = pkgs.firefox-unwrapped;
    };
    texlive = {
      enable = true;
      extraPackages = tpkgs: {
        inherit (tpkgs)
          scheme-full
          # scheme-medium
          collection-fontsrecommended
          algorithms
          latexmk;
          # xelatex;
      };
    };
    ssh    = import ./programs/ssh.nix;
    fzf    = import ./programs/fzf.nix;
    git    = import ./programs/git.nix;
    tmux   = import ./programs/tmux.nix { inherit pkgs; };
    urxvt  = import ./programs/urxvt.nix { inherit pkgs; };
    neovim = neovim.config;
    jq = {
      enable = true;
      colors = {
        null    = "1;30";
        false   = "0;31";
        true    = "0;32";
        numbers = "0;36";
        strings = "0;33";
        arrays  = "1;35";
        objects = "1;37";
      };
    };
  } // mail.programs;

  accounts.email = mail.email;

  dconf  = {
    enable = true;
    settings = { };
  };

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

  # systemd.user.services.qsyncthingtray.Unit.After         = [ "taffybar.service" ] ++ systemd.user.services.qsyncthingtray.Unit.After;
  # systemd.user.services.flameshot.Unit.After              = [ "taffybar.service" ] ++ systemd.user.services.flameshot.Unit.After;
  # systemd.user.services.redshift.Unit.After               = [ "taffybar.service" ] ++ systemd.user.services.redshift.Unit.After;
  # systemd.user.services.pasystray.Unit.After              = [ "taffybar.service" ];
  # systemd.user.services.parcellite.Unit.After             = [ "taffybar.service" ];
  # systemd.user.services.network-manager-applet.Unit.After = [ "taffybar.service" ];

  systemd.user.services.offlineimap = {
    Unit = {
      Description = "Offlineimap service";
      Requires = [ "protonmail-bridge.service" ];
      After    = [ "protonmail-bridge.service" ];
    };

    Service = {
      ExecStart ="${pkgs.offlineimap}/bin/offlineimap";
      Restart = "on-failure";
      RestartSec = "5s";
    };

    Install = {
      WantedBy = [ "default.target" ];
    };
  };

  systemd.user.services.sovushka-server = {
    Unit = {
      Description = "Sovushka server service";
    };
    Service = {
      ExecStart ="sovushka-server";
    };
    Install = {
      WantedBy = [ "default.target" ];
    };
  };
}

