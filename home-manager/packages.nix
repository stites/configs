{ pkgs, lib, config, ... }:

let
  host = pkgs.callPackage ./hosts.nix { };
  stable = pkgs.stable;
  unstable = pkgs;

  elmPackages = stable.elmPackages;
  haskellPackages844 = pkgs.haskellPackages.packages.ghc844;
  firstHomeManagerBoot = !(builtins.pathExists "${builtins.getEnv "HOME"}/.config/nixpkgs/config.nix");

  # pyls = (with unstable.python3Packages; python-language-server.override {
  #   autopep8 = autopep8;
  #   mccabe = mccabe;
  #   pycodestyle = pycodestyle;
  #   pydocstyle = pydocstyle;
  #   pyflakes = pyflakes;
  #   rope = rope;
  #   yapf = yapf;
  # });

  # RStudio-with-packages = unstable.rstudioWrapper.override { packages = with unstable.rPackages; [
  #   xts
  #   rstan
  #   ggplot2
  #   ggvis
  #   rgl
  #   shiny
  #   zoo
  #   # parallel
  #   jug
  #   data_table
  #   dplyr
  #   tidyr
  #   stringr
  #   lubridate
  # ]; };

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
      colordiff
      unzip

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
      (exe haskellPackages.alex)
      (exe haskellPackages.happy)
      (exe haskellPackages.hpack)
      (exe haskellPackages.hindent)
      (exe haskellPackages.hoogle)
      (exe haskellPackages.pointfree)
      (exe haskellPackages.hasktags)
      (exe haskellPackages.hspec-discover)
      (exe haskellPackages.ghcid)

      # extra git stuff
      tig
      git-radar
      git-secrets # FIXME: add "install hook if you own the repository" warning on cd
      gitAndTools.git-extras
      gitAndTools.diff-so-fancy
      gitAndTools.hub
      gitAndTools.git-annex

      # profiling tools
      hyperfine
      flamegraph

      # nix
      nix-prefetch-git
      nix-serve
      nix-bash-completions
      nix-info
      nix-index
      (exe haskellPackages.stack2nix)
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
      nim
      xdotool
      pstree
      # texlive.combined.scheme-full
      # weechat # << install 2.4-devel version
      bash-completion

      # cachix  # << install via cachix.com

      # In order to build things outside of nix
      zlib.out # otherwise this goes to bin
      zlib.dev # needed for pyenv
      openssl.out
      openssl.dev
      bzip2.out
      bzip2.dev
      sqlite.out
      sqlite.dev
      readline.out
      readline.dev
      libffi.out
      libffi.dev

    ]) ++ (if host.isServer then (with stable; [ znc ]) else []);
       # ++ (if firstHomeManagerBoot then [] else (with unstable; [ ngrok ]));


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
      # weechat
      watchexec
      (exe haskellPackages.glirc)

      # BROKEN
      # (exe haskellPackages.git-monitor)
      # (exe haskellPackages.nvim-hs-ghcid)
      # (exe haskellPackages.threadscope)
      # (exe haskell.packages.ghc844.patat)
      # (exe haskell.packages.ghc844.codex)
    ]);

  unstableNixOS =
    if host.isNixOS
    then (with unstable; [
        coreutils
        ffmpeg

        # fonts
        fira
        powerline-fonts
        # nerdfonts # this takes >30m to download

        # GUI TOOLS (needs to be changed to nixos-only)
        # signal-desktop
        # stable.gitter
        slack
        spotify

        # migrate to home-manager
        # keybase
        # keybase-gui

        # compute stuff
        liblapack
        # mkl      # <<< look to llvm for openmp
        # RStudio-with-packages

        # pythonEnvWithCuda
        conda

        # c/cpp dev
        valgrind bazel
        protobuf
        # glxinfo

        # enter the clang
        ncurses
        llvmPackages_7.clang
        llvmPackages_7.clang-manpages
        llvmPackages_7.compiler-rt
        llvmPackages_7.lld
        llvmPackages_7.lldb
        llvmPackages_7.llvm
        llvmPackages_7.llvm-manpages
        llvmPackages_7.openmp
        # llvmPackages_7.stdenv
        rtags
        # ccls # clang/llvm C++17 language server, uses clang-index

        # c/cpp code analysis
        clang-tools
        cpplint
        cppcheck
        include-what-you-use
        uncrustify             # code formatter

        # cuda-shell
    ]) else [];

in
  {
    home.packages = stableNixPkgs ++ unstableNixPkgs ++ unstableNixOS;
  }

