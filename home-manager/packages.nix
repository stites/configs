{ pkgs, lib, config, ... }:

let
  host = import ./hosts.nix { inherit pkgs lib config; };
  stable = pkgs.stable;
  unstable = pkgs;

  elmPackages = stable.elmPackages;
  haskellPackages844 = pkgs.haskellPackages.packages.ghc844;

  RStudio-with-packages = unstable.rstudioWrapper.override { packages = with unstable.rPackages; [
    xts
    rstan
    ggplot2
    ggvis
    rgl
    shiny
    zoo
    # parallel
    jug
    data_table
    dplyr
    tidyr
    stringr
    lubridate
  ]; };

  my-dictionaries = with pkgs; buildEnv {
    name = "my-dictionary";
    paths = [
      dict
      # dictdDBs.eng2rus
      dictdDBs.wiktionary
      dictdDBs.wordnet

      aspell
      aspellDicts.en
    ];
  };

  stableNixPkgs =
    let exe = stable.haskell.lib.justStaticExecutables;
    in (with stable; [
      fortune
      cowsay
      coreutils
      neofetch
      calibre

      finger_bsd
      cloc
      socat
      xsv
      cmake
      curl
      ncdu
      fd
      xclip
      ripgrep
      gawk
      less
      lesspipe
      most
      watch
      xz
      unar
      aircrack-ng
      youtube-dl
      pmutils
      syncthing
      zip
      gnused
      # gl # sed for json
      entr # when sos fails us
      mosh
      shellcheck
      par
      tree
      wget
      fasd
      httpie
      exa

      # email
      notmuch-mutt
      neomutt
      lynx
      xpdf # view pdf in the terminal via pdftotext
      w3m # view html in the terminal

      # dev tools
      vagrant
      tldr
      asciinema
      gotty
      graphviz
      sqlite
      sqliteman

      # haskell
      hledger
      hledger-ui
      hledger-web
      hlint
      (exe haskellPackages.shake)
      # haskellPackages.shake-extras
      (exe haskellPackages.alex)
      (exe haskellPackages.brittany)
      (exe haskellPackages.happy)
      (exe haskellPackages.hpack)
      (exe haskellPackages.pointfree)
      (exe haskellPackages.hasktags)
      (exe haskellPackages.hspec-discover)
      (exe haskellPackages.ghcid)
      (exe haskellPackages.patat)
      (exe haskellPackages.nvim-hs-ghcid)
      # (exe haskellPackages.threadscope)
      # (exe haskellPackages.glirc)
      # (exe pkgs.haskell.packages.ghc822.codex)

      # extra git stuff
      tig
      git-lfs
      git-radar
      gitAndTools.git-extras
      gitAndTools.diff-so-fancy
      gitAndTools.hub
      (exe haskellPackages.git-monitor)
      (exe haskellPackages.git-annex)

      # profiling tools
      hyperfine
      flamegraph

      # nix
      nix-prefetch-git
      nix-serve
      nix-bash-completions
      nix-info
      nix-index
      # (exe haskellPackages.stack2nix)
      (exe haskellPackages.cabal2nix)

      # security tools
      yubico-piv-tool
      yubikey-manager
      yubikey-personalization
      gnupg22

      bench
      ddgr
      dhall-json
      gv
      imagemagick
      file
      macchanger
      ngrok
      nim
      xdotool
      pstree
      # texlive.combined.scheme-full
      # weechat # << install 2.4-devel version
      bash-completion

      cachix
      hies
    ]) ++ (if host.isServer then [ pkgs.znc ] else []);


  unstableNixPkgs =
    let exe = unstable.haskell.lib.justStaticExecutables;
    in (with unstable; [
      cabal-install
      stack
      noti
      bat
      rxvt_unicode_with-plugins
      protonmail-bridge
      prettyping
      # nix-linting
      my-dictionaries
      # zoom-us
      weechat

      # In order to build things outside of nix
      zlib
      zlib.out
      zlib.dev
      openssl
      openssl.out
      openssl.dev
      bzip2
      bzip2.out
      bzip2.dev
      sqlite.out
      sqlite.dev
      readline.out
      readline.dev
      gcc7.out
    ]);

  unstableNixOS =
    if host.isNixOS
    then (with unstable; [
        # fonts
        fira
        powerline-fonts
        # nerdfonts # this takes >30m to download

        # GUI TOOLS (needs to be changed to nixos-only)
        signal-desktop
        stable.gitter
        slack

        # migrate to home-manager
        keybase
        keybase-gui

        # compute stuff
        liblapack
        RStudio-with-packages
    ]) else [];

in stableNixPkgs ++ unstableNixPkgs ++ unstableNixOS

