{ pkgs, lib, config, ... }:

let
  stdenv = pkgs.stdenv;
  homedir = builtins.getEnv "HOME";
  ca-bundle_crt = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt"; # just in case
  lib = stdenv.lib;
  concatStringsSep = lib.strings.concatStringsSep;
  neovim = pkgs.callPackage ./programs/nvim { };
  mail = pkgs.callPackage ./mail.nix { };
  host = pkgs.callPackage ./hosts.nix { };
  secrets = import ./secrets.nix;
  hpkgs822 = pkgs.haskell.packages.ghc822;
  # hpkgs822 = pkgs.haskell.packages.ghc822.extend (sel: sup: {
  #   mkDerivation = drv: sup.mkDerivation (drv // { doHaddock = false; }); # jailbreak = true; });
  # });
  optionalString = lib.optionalString;
in
{
  manual.html.enable = true;
  manual.manpages.enable = true;

  home = {
    sessionVariables = {
      # so that electron apps play nicely with taffybar
      XDG_CURRENT_DESKTOP = "Unity";
      # GDK_SCALE=2;
      # GDK_DPI_SCALE="0.5";
    };
    packages = (import ./packages.nix { inherit pkgs lib config; });
      # ++ [ pkgs.protonmail-bridge pkgs.screen  hpkgs822.taffybar ];
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

    file.".jupyter/jupyter_notebook_config.py".text = (pkgs.callPackage ./programs/jupyter/jupyter_notebook_config.nix {}).text;
    file.".aspell.en.pws".source = "${homedir}/git/configs/home-manager/my-aspell-ws";
    file.".aspell.en.prepl".source = "${homedir}/git/configs/home-manager/my-aspell-repl";
    file.".tmuxifier" = {
      recursive = true;
      source = pkgs.fetchFromGitHub {
        owner = "jimeh";
        repo = "tmuxifier";
        rev = "v0.13.0";
        sha256 = "1b6a1cw2mnml84k5vhbcp58kvp94xlnlpp4kwdhqw4jrzfgcjfzd";
      };
      onChange =
        let
          mkide = ishs: concatStringsSep "\n" [
            # Set window root path. Default is `$session_root`.
            # Must be called before `new_window`.
            # ESCAPED BECAUSE WE ARE PIPING TO STDIN TO GENERATE THESE FILES
            ''window_root "\$(pwd)"''

            # Create new window. If no argument is given, window name will be based on
            # layout file name.
            # ESCAPED BECAUSE WE ARE PIPING TO STDIN TO GENERATE THESE FILES
            ''new_window "\$(pwd | sed 's@.*/@@')"''

            # |--------- ---------|
            # |         |         |
            # |         |    2    |
            # |    1    |---------|
            # |         |         |
            # |         |    3    |
            # |         |---------|
            # |         |    4    |
            # |         |---------|
            # |         |    5    |
            # |         |---------|
            # |         |    6    |
            # |--------- ---------|

            # Split window into panes.
            "split_h"
            # select_pane 1
            # split_v 8
            "select_pane 2"
            "split_v"
            "split_v"
            "split_v"
            (if ishs then "split_v" else "")

            # Run commands.
            (if ishs
              then ''run_cmd "vim Setup.hs +Ghcid" 1''
              else ''run_cmd "vim" 1'')

            # run_cmd "make ghcid && echo \"ghcid ran from make\"
                   #  || ghcid --height=8 --command=\"stack ghci --test\"" 2
            ''run_cmd "git status"  2''
            ''run_cmd "${if ishs then "stack ghci --test" else "ipython"}"     3''
            (if ishs
              then ''run_cmd "sos -p '([^_].*\.py)'               -c 'python \1'" 4''
              else ''run_cmd "sos -p 'app/.*\.hs' -p 'src/.*\.hs' -c 'hlint  \0'" 4'')
            (if ishs
              then ''run_cmd "sos -p '([^_].*\.py)' -c 'ctags -R --extra=+f --python-kinds=-i'" 5''
              else ''run_cmd "sos -p '.*\.hs'       -c 'codex update --force'"                  5'')
            ''run_cmd "vim .stites" 6''

            # Set active pane.
            "select_pane 1"
          ];
          hside = mkide true;
          pyide = mkide false;
        in
        ''
          cat >${homedir}/.tmuxifier/layouts/py.window.sh <<EOL
          ${pyide}
          EOL
          cat >${homedir}/.tmuxifier/layouts/hs.window.sh <<EOL
          ${hside}
          EOL
        '';
    };
    file.".ghci".source                            = ./haskell/configs/ghci;
    file.".haskline".source                        = ./haskell/configs/haskline;
    file.".stack/config.yaml".source               = ./haskell/configs/stack-local.yaml;
    file.".stack/global-project/stack.yaml".source = ./haskell/configs/stack-global.yaml;
  };

  nixpkgs.config = (import ./config.nix { inherit pkgs lib config; });

  xdg = {
    enable = true;
    configFile = {
      "nixpkgs/isNixOS".text = "${if host.isNixOS then "true" else "false"}";
      "nixpkgs/config.nix".source = "${homedir}/git/configs/home-manager/config.nix";
      "termonad/termonad.hs".source = ./programs/termonad/termonad.hs;
      "lsp/settings.json".text = ''
        {
          "languageServerHaskell": {
            "hlintOn": true,
            "maxNumberOfProblems": 10,
            "useCustomHieWrapper": true,
            "useCustomHieWrapperPath": "hie-wrapper"
          }
        }
      '';
      # "nixpkgs/local-nixpkgs".source      = "${homedir}/git/configs/nixpkgs";
      "nixpkgs/signal-desktop-beta.nix".source = "${homedir}/git/configs/home-manager/signal-desktop-beta.nix";
      # "taffybar/taffybar.hs" = {
      #   source = ./xmonad/taffybar.hs;
      #   onChange = "rm -rf ${homedir}/.cache/taffybar/";
      # };
      # "taffybar/taffybar.css" = {
      #   source = ./xmonad/taffybar.css;
      #   onChange = "rm -rf ${homedir}/.cache/taffybar/";
      # };
      "nvim/UltiSnips/python.snippets" = {
        source = ./programs/nvim/UltiSnips/python.snippets;
      };
      "glirc/config".text = (import ./programs/glirc/config.nix {}).config;
      "bat/config".text = ''
        --theme="TwoDark"
        --italic-text=never

        # Show line numbers, Git modifications and file header (but no grid)
        --style="numbers,changes,header,grid"

        # Add mouse scrolling support in less (does not work with older
        # versions of "less").
        --pager="less -FR"

        # Use C++ syntax (instead of C) for .h header files
        --map-syntax h:cpp

        # Use "gitignore" highlighting for ".ignore" files
        --map-syntax .ignore:.gitignore
      '';

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
          options_ghc:
          - -XLambdaCase
          - -XMultiWayIf
          - -XGADTs
          - -XPatternGuards
          - -XViewPatterns
          - -XRecursiveDo
          - -XTupleSections
          - -XExplicitForAll
          - -XImplicitParams
          - -XQuasiQuotes
          - -XTemplateHaskell
          - -XBangPatterns
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
          (builtins.readFile ./programs/nvim/UltiSnips/haskell.snippets)
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
        config = ./xmonad/xmonad.hs;
        # haskellPackages = hpkgs822;
        # extraPackages = hpkgs: with hpkgs [
        #   taffybar
        # ];
      };
    };
  };


  gtk = {
    enable = true;
    gtk3 = {
      extraConfig = {
        scaling-factor = 1;
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

  services.xembed-sni-proxy.enable = true;
  services.flameshot.enable = true;

  services.syncthing = {
    enable = false;
    tray = false;
  };

  services.udiskie = {
    enable = true;
    tray = "auto";
  };

  services.redshift ={
    enable = false;
    tray = true;
    brightness.night = "0.5";
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
    enableExtraSocket = true;
    maxCacheTtl       = 60480000;
    defaultCacheTtl   = 60480000;
    extraConfig = ''
      allow-preset-passphrase
    '';
  };
  services.blueman-applet.enable = true; # requires system install
  # services.dunst.enable = false;          # notification daemon
  services.network-manager-applet.enable = true;
  services.pasystray.enable = true;         # PulseAudio system tray
  services.parcellite.enable = true;        # clipboard daemon
  services.compton.enable = true;           # needs configuration
  # services.protonmail-bridge.enable = true;

  programs = {
    rofi = {
      enable = true;
      borderWidth = 1;
      terminal = "urxvt";
      theme = "Arc";
    };
    home-manager = {
      enable = true;
      path = "$HOME/git/home-manager";
    };

    bash = import ./programs/bash { inherit lib pkgs; };
    keychain = {
      enable = true;
      enableBashIntegration = true;
      enableZshIntegration = false;
      agents = [ "ssh" "gpg" ];
      inheritType = null;
      keys = [ "id_rsa" "id_ed25519" secrets.gpg.signing-key ];
    };
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
    # Note that this doesn't install matplotlib, it only configures the global properties of it.
    matplotlib = {
      # Whether to enable matplotlib, a plotting library for python.
      enable = true;
      # Add terms to the matplotlibrc file to control the default matplotlib behavior.
      config = {
        backend = "Qt5Agg";
        axes = {
          grid = true;
          facecolor = "black";
          edgecolor = "FF9900";
        };
        grid.color = "FF9900";
      };
      # Additional commands for matplotlib that will be added to the matplotlibrc file.
      extraConfig = "";
    };
  } // mail.programs;

  accounts.email = mail.email;

  dconf  = {
    enable = true;
    settings = { };
  };

  # systemd.user.services.parcellite.Unit.After             = [ "taffybar.service" ];
  # systemd.user.services.network-manager-applet.Unit.After = [ "taffybar.service" ];

  # systemd.user.services.offlineimap = {
  #   Unit = {
  #     Description = "Offlineimap service";
  #     Requires = [ "protonmail-bridge.service" ];
  #     After    = [ "protonmail-bridge.service" ];
  #   };

  #   Service = {
  #     ExecStart ="${pkgs.offlineimap}/bin/offlineimap";
  #     Restart = "on-failure";
  #     RestartSec = "5s";
  #   };

  #   Install = {
  #     WantedBy = [ "default.target" ];
  #   };
  # };

  # systemd.user.services.sovushka-server = {
  #   Unit = {
  #     Description = "Sovushka server service";
  #   };
  #   Service = {
  #     ExecStart ="sovushka-server";
  #   };
  #   Install = {
  #     WantedBy = [ "default.target" ];
  #   };
  # };
}

