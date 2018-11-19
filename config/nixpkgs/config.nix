{
  allowUnfree = true;
  packageOverrides = pkgs_: with pkgs_; {
    my-dictionary = with pkgs; buildEnv {
      name = "my-dictionary";
      paths = [
        dict
        # dictdDBs.eng2rus
        dictdDBs.wiktionary
        dictdDBs.wordnet
      ];
    };

    all = with pkgs; let exe = haskell.lib.justStaticExecutables; in buildEnv {
      name = "all";
      paths = [
        # window manager (maybe desktop manager)
        # taffybar
        # haskellPackages.xmobar
        # haskellPackages.xmonad
        # haskellPackages.xmonad-contrib

        # system-level GUI deps
        albert
        signal-desktop
        firefox-devedition-bin
        gitter
        slack
        # mendeley # broken

        # keybase     # <<< needs some more coordination
        # keybase-gui # <<< needs some more coordination

        # system level deps
        bash-completion
        bashInteractive

        # dev tools
        gotty
        graphviz
        sqlite
        sqliteman
      ];
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
