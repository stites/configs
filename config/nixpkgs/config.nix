{
  allowUnfree = true;

  # firefox = {
  #   enableAdobeFlash = true;
  #   enableGoogleTalkPlugin = true;
  # };

  packageOverrides = pkgs_: with pkgs_; {

    all = with pkgs; let exe = haskell.lib.justStaticExecutables; in buildEnv {
      name = "all";
      paths = [
        # system-level GUI deps
        albert
        signal-desktop
        firefox-devedition-bin
        gitter
        slack

        # system level deps
        bash-completion
        bashInteractive
        bat
        cmake
        curl
        ncdu
        fd
        xclip
        ripgrep
        rxvt_unicode_with-plugins
        aspell
        aspellDicts.en
        gawk
        less
        more
        watch
        xz

        tmux
        tmuxinator
        tree
        wget
        fasd
        fzf
        htop
        httpie
        # prettyping # <<< not in nixpkgs
        tldr
        jq

        # dev tools
        emacs
        vagrant
        vim
        neovim
        gotty
        graphviz

        # elm
        elmPackages.elm
        elmPackages.elm-format

        # git stuff
        git
        tig
        git-lfs
        git-radar
        gitAndTools.git-extras
        gitAndTools.diff-so-fancy
        gitAndTools.hub
        # (exe haskellPackages.git-monitor)

        # haskell
        hledger
        cabal-install
        hlint
        haskellPackages.shake
        # haskellPackages.shake-extras
        (exe haskellPackages.alex)
        (exe haskellPackages.happy)
        (exe haskellPackages.hpack)
        (exe haskellPackages.pointfree)
        (exe haskellPackages.hasktags)
        (exe haskellPackages.hspec-discover)
        (exe haskellPackages.ghcid)
        (exe haskellPackages.nvim-hs-ghcid)

        # CLI benchmarking
        hyperfine

        # nix
        nix-prefetch-git
        nix-serve
        nix-bash-completions
        nix-info

        # yubico, unexplored tools
        yubico-piv-tool
        yubikey-manager
        yubikey-personalization

        # fonts
        fira
        powerline-fonts
      ];
    };

    haskellEnv = buildEnv {
      name = "haskellEnv";
      paths = [ python36 neovim cabal-install ];
    };

    neovim3Env = buildEnv {
      name = "neovim3Env";
      paths = [ python36 neovim ];
    };
  };
}
