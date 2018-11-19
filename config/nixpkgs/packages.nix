{ pkgs, ... }:

with pkgs;

let
  elmPackages = pkgs.elmPackages;
  haskellPackages = pkgs.haskellPackages;
  rPackages = pkgs.rPackages;
  exe = pkgs.haskell.lib.justStaticExecutables;

  RStudio-with-packages = rstudioWrapper.override { packages = with rPackages; [
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
in
[
  fortune
  cowsay
  coreutils
  neofetch

  # alacritty
  noti
  cloc
  bat
  socat
  xsv
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
  lesspipe
  most
  watch
  xz
  aircrack-ng
  youtube-dl
  pmutils
  syncthing
  zip
  gnused
  # gl # sed for json
  entr # for when sos fails us
  mosh
  shellcheck
  par

  # email
  protonmail-bridge
  notmuch
  notmuch-mutt
  neomutt
  offlineimap
  lynx

  tree
  wget
  fasd
  fzf
  htop
  httpie
  exa
  prettyping

  tldr
  jq

  # dev tools
  vagrant
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
  tig
  git-lfs
  git-radar
  gitAndTools.git-extras
  gitAndTools.diff-so-fancy
  gitAndTools.hub
  (exe haskellPackages.git-monitor)

  # haskell
  hledger
  cabal-install
  stack
  hlint
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

  RStudio-with-packages

  # profiling tools
  hyperfine
  flamegraph

  # nix
  nix-prefetch-git
  nix-serve
  nix-bash-completions
  nix-info
  # (exe haskellPackages.stack2nix)
  (exe haskellPackages.cabal2nix)

  # security tools
  yubico-piv-tool
  yubikey-manager
  yubikey-personalization
  gnupg22

  # fonts
  fira
  powerline-fonts
  # nerdfonts
  asciinema

  bench
  cachix
  ddgr
  dhall-json
  gv
  imagemagick
  macchanger
  ngrok
  nim
  scrot
  xdotool
  pstree
  texlive.combined.scheme-full
  weechat
]

