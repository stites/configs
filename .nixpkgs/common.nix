{pkgs}:
let
  vimrc = import ./vimrc.nix {};
in
with pkgs; rec {
  setPrio = num: drv: lib.addMetaAttrs { priority = num; } drv;
  haskellngPackages = pkgs.haskellngPackages;
  lushtags = pkgs.haskell.lib.overrideCabal haskellPackages.lushtags (oldAttrs: {
    src = pkgs.fetchgit {
      url = https://github.com/mkasa/lushtags;
      rev = "3d7229b07b47ab2fc188d7db63b33dfcd63a1802";
      sha256 = "1f87zjs9d03h3g4r93ad4asdyx5jsx7nyzf5al72s855adxypdpm";
    };
  });

  vimrcConfig = {
        vam.knownPlugins = vimPlugins; # optional
        vam.pluginDictionaries = [
            #check ftdetect bug
            { names = [
                "ghcmod"
                "haskellconceal"
                "hoogle"
                "lushtags"
                "neco-ghc"
                "rust"
                "airline"
                "colors-solarized"
                "ctrlp"
                "easy-align"
                "easymotion"
                "fugitive"
                "gitgutter"
                "vim-hardtime"
                "vim-hier"
                "nerdcommenter"
                "nerdtree"
                "quickfixstatus"
                "quickrun"
                "rainbow_parentheses"
                "surround"
                "tabmerge"
                "tagbar"
                "taglist"
                "thumbnail-vim"
                "undotree"
                "vimproc-vim"
                "vim-addon-nix"
                "vim-gista"
                "vim-wakatime"
                "watchdogs"
                "syntastic"
                "webapi-vim"
                "vim-xkbswitch"
                "youcompleteme"
            ];}
        ];
        customRC = vimrc.config;
      };
      my_vim = vim_configurable.customize { name = "vim"; inherit vimrcConfig; };

      vimEnv = lib.lowPrio (
        buildEnv {
          name = "vim-env";
          ignoreCollisions = true;
          paths = [
              my_vim
              /*racerRust*/
              haskellngPackages.stylish-haskell
              astyle
            ];
        }
      );

      emacsEnv = setPrio "9" (
        buildEnv {
          name = "emacs-env";
          ignoreCollisions = true;
          paths = [
          (emacsWithPackages (with emacsPackages; with emacsPackagesNg; [
              company
              company-ghc
              evil
              evil-leader
              #evil-surround
              flycheck
              haskell-mode
              helm
              markdown-mode
              monokai-theme
              org
              rainbow-delimiters
              undo-tree
              use-package
            ]))
          ];
        }
      );

      baseEnv = lib.lowPrio (
        buildEnv {
          name = "base-env";
          ignoreCollisions = true;
          paths = [ 
            androidenv.platformTools
            perlPackages.ack
            aspell
            aspellDicts.en
            aspellDicts.ru
            bc
            defaultStdenv
            djview4
            evince
            file
            flac
            flashplayer
            freetype
            gajim
            dejavu_fonts
            gnome.zenity
            gnupg
            gparted
            iftop
            lastfmsubmitd
            libnotify
            lm_sensors
            mocPulse
            mutt
            nethogs
            p7zip
            pass
            pinentry
            psmisc
            qutebrowser
            skype
            sxiv
            telnet
            tightvnc
            tmux
            traceroute
            tree
            unrar
            unzip
            usbutils
            vlc
            weechat
            which
            wineUnstable
            winetricks
            wmname
            /* xclip */
            /* xkblayout-state */
            /* xlibs.xev */
            /* xlibs.xprop */
            /* xfce.xfce4notifyd */
            /* xfce.xfce4terminal */
            zip
          ];
        }
      );

      myHs = haskellngPackages.ghcWithPackages (
        pkgs: with pkgs; [
            cabal2nix
            cabal-install
            ghc-mod
            hlint
            hoogle
            lushtags
            pandoc
            taffybar
            xmonad
            xmonad-contrib
        ]);

      hsEnv = buildEnv {
        name = "haskell-env";
        ignoreCollisions = true;
        paths = [
            myHs
            loveMoc
        ];
      };

      develEnv = lib.lowPrio (
        pkgs.buildEnv {
          name = "development-env";
          ignoreCollisions = true;
          paths = [
              automake
              cargoSnapshot
              clang
              cmake
              ctags
              freeglut
              gdb
              gcc
              gnumake
              /*haskellngPackages.idris*/
              idea.idea-community
              jdk
              manpages
              mercurial
              mesa
              pciutils
              pkgconfig
              python
              python34
              ruby
              /*rustc*/
              smartmontools
              sqlite
              subversion
              swiProlog
              wireshark
              xlibs.libX11
              zlib
          ];
        }
      );
}
