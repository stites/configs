{ pkgs, ... }:

with pkgs;

let
  elmPackages = unstable.elmPackages;
  # haskellPackages = pkgs.haskellPackages;
  exe = haskell.lib.justStaticExecutables;

  RStudio-with-packages = unstable.rstudioWrapper.override { packages = with rPackages; [
    xts
    rstan
    ggplot2
    ggvis
    rgl
    shiny
    zoo
    parallel
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
      unstable.dict
      # dictdDBs.eng2rus
      unstable.dictdDBs.wiktionary
      unstable.dictdDBs.wordnet

      unstable.aspell
      unstable.aspellDicts.en
    ];
  };
in
[
  unstable.fortune
  unstable.cowsay
  unstable.coreutils
  unstable.neofetch

  # alacritty
  unstable.noti
  unstable.cloc
  unstable.bat
  unstable.socat
  unstable.xsv
  unstable.cmake
  unstable.curl
  unstable.ncdu
  unstable.fd
  unstable.xclip
  unstable.ripgrep
  unstable.rxvt_unicode_with-plugins
  unstable.gawk
  unstable.less
  unstable.lesspipe
  unstable.most
  unstable.watch
  unstable.xz
  unstable.aircrack-ng
  unstable.youtube-dl
  unstable.pmutils
  unstable.syncthing
  unstable.zip
  unstable.gnused
  # gl # sed for json
  unstable.entr # for when sos fails us
  unstable.mosh
  unstable.shellcheck
  unstable.par

  # email
  unstable.protonmail-bridge
  #stable.notmuch
  unstable.notmuch-mutt
  unstable.neomutt
  #stable.offlineimap
  unstable.lynx
  unstable.xpdf # view pdf in the terminal via pdftotext
  unstable.w3m # view html in the terminal

  unstable.tree
  unstable.wget
  unstable.fasd
  # unstable.htop
  unstable.httpie
  unstable.exa
  prettyping

  unstable.tldr
  unstable.jq

  # dev tools
  unstable.vagrant
  # vim
  # neovim
  # gotty
  # graphviz
  # sqlite
  # sqliteman

  # elm
  elmPackages.elm
  elmPackages.elm-format

  # extra git stuff
  unstable.tig
  unstable.git-lfs
  unstable.git-radar
  unstable.gitAndTools.git-extras
  unstable.gitAndTools.diff-so-fancy
  unstable.gitAndTools.hub
  (exe haskellPackages.git-monitor)

  # haskell
  unstable.hledger
  unstable.cabal-install
  unstable.stack
  unstable.hlint
  (exe haskellPackages.shake)
  # haskellPackages.shake-extras
  (exe haskellPackages.alex)
  (exe haskellPackages.happy)
  (exe haskellPackages.hpack)
  (exe haskellPackages.pointfree)
  (exe haskellPackages.hasktags)
  (exe haskellPackages.hspec-discover)
  (exe haskellPackages.ghcid)
  (exe haskellPackages.patat)
  (exe haskellPackages.nvim-hs-ghcid)
  # (exe haskellPackages.glirc)
  # (exe pkgs.haskell.packages.ghc822.codex)

  RStudio-with-packages

  # profiling tools
  unstable.hyperfine
  unstable.flamegraph

  # nix
  unstable.nix-prefetch-git
  unstable.nix-serve
  unstable.nix-bash-completions
  unstable.nix-info
  # (exe haskellPackages.stack2nix)
  (exe haskellPackages.cabal2nix)

  # security tools
  unstable.yubico-piv-tool
  unstable.yubikey-manager
  unstable.yubikey-personalization
  unstable.gnupg22

  # fonts
  unstable.fira
  unstable.powerline-fonts
  # nerdfonts # this takes >30m to download
  unstable.asciinema

  unstable.bench
  unstable.cachix
  unstable.ddgr
  unstable.dhall-json
  unstable.gv
  unstable.imagemagick
  unstable.file
  unstable.macchanger
  unstable.ngrok
  unstable.nim
  unstable.xdotool
  unstable.pstree
  stable.texlive.combined.scheme-full
  # weechat # << install 2.4-devel version
  unstable.bash-completion
  my-dictionaries

  # dev tools
  unstable.gotty
  unstable.graphviz
  unstable.sqlite
  unstable.sqliteman

  # GUI TOOLS (needs to be changed to nixos-only)
  signal-desktop
  unstable.firefox-devedition-bin
  unstable.gitter
  slack
  unstable.mendeley

  # migrate to home-manager
  unstable.keybase
  unstable.keybase-gui

  # compute stuff
  unstable.liblapack
]

