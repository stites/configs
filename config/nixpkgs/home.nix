{ pkgs, ... }:

let
  homedir = builtins.getEnv "HOME";
  ca-bundle_crt = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt"; # just in case
  lib = pkgs.stdenv.lib;
  exe = pkgs.haskell.lib.justStaticExecutables;
in
{
  home = {
    packages = [
      pkgs.fortune
      pkgs.cowsay

      pkgs.bat
      pkgs.cmake
      pkgs.curl
      pkgs.ncdu
      pkgs.fd
      pkgs.xclip
      pkgs.ripgrep
      pkgs.rxvt_unicode_with-plugins
      pkgs.aspell
      pkgs.aspellDicts.en
      pkgs.gawk
      pkgs.less
      pkgs.lesspipe
      pkgs.most
      pkgs.watch
      pkgs.xz
      pkgs.aircrack-ng
      pkgs.youtube-dl
      pkgs.pmutils
      pkgs.syncthing
      pkgs.zip
      pkgs.gnused
      # gl # sed for json
      pkgs.entr # for when sos fails us
      pkgs.mosh

      # tmuxinator
      # tmux-bundled
      # tmuxPlugins.battery
      # tmuxPlugins.continuum
      # tmuxPlugins.cpu
      # tmuxPlugins.fzf-tmux-url
      # tmuxPlugins.resurrect
      # tmuxPlugins.sensible

      pkgs.tree
      pkgs.wget
      pkgs.fasd
      pkgs.fzf
      pkgs.htop
      pkgs.httpie
      pkgs.exa
      # prettyping # <<< not in nixpkgs

      pkgs.tldr
      pkgs.jq

      # # dev tools
      # vagrant
      # vim
      # neovim
      # gotty
      # graphviz
      # sqlite
      # sqliteman

      # # elm
      # elmPackages.elm
      # elmPackages.elm-format

      # # git stuff
      # git
      pkgs.tig
      pkgs.git-lfs
      pkgs.git-radar
      pkgs.gitAndTools.git-extras
      pkgs.gitAndTools.diff-so-fancy
      pkgs.gitAndTools.hub
      # (exe pkgs.haskellPackages.git-monitor)

      # # haskell
      # hledger
      # cabal-install
      # hlint
      # haskellPackages.shake
      # # haskellPackages.shake-extras
      # (exe haskellPackages.alex)
      # (exe haskellPackages.happy)
      # (exe haskellPackages.hpack)
      # (exe haskellPackages.pointfree)
      # (exe haskellPackages.hasktags)
      # (exe haskellPackages.hspec-discover)
      # (exe haskellPackages.ghcid)
      # (exe haskellPackages.nvim-hs-ghcid)

      # # CLI benchmarking
      # hyperfine

      # nix
      pkgs.nix-prefetch-git
      pkgs.nix-serve
      pkgs.nix-bash-completions
      pkgs.nix-info

      # # yubico, unexplored tools
      # yubico-piv-tool
      # yubikey-manager
      # yubikey-personalization

      # # fonts
      pkgs.fira
      pkgs.powerline-fonts
    ];
  };

  # imports = [
  #   ./modules/tmux
  # ];

  programs = {
    home-manager = {
      enable = true;
      path = https://github.com/rycee/home-manager/archive/master.tar.gz;
    };


    #git = {
    #  enable = true;
    #  userName  = "Sam Stites";
    #  userEmail = "sam@stites.io";

    #  #signing = {
    #  #  key = "..";
    #  #  signByDefault = true;
    #  #};
    #};
  };


}

