# REFERENCES:
# https://nixos.org/nixos/manual/index.html#sec-building-cd (accessed 08/13/19: https://perma.cc/KU5J-V36W )
# https://nixos.wiki/wiki/Creating_a_NixOS_live_CD          (accessed 08/13/19: https://perma.cc/HUN7-ZZHB )

# This module defines a small NixOS installation CD.  It does not
# contain any graphical stuff.
{config, pkgs, ...}:
let
  use_retina = false;
in
{
  imports = [
    <nixpkgs/nixos/modules/installer/cd-dvd/installation-cd-graphical-gnome.nix>

    # Provide an initial copy of the NixOS channel so that the user
    # doesn't need to run "nix-channel --update" first.
    <nixpkgs/nixos/modules/installer/cd-dvd/channel.nix>
  ];

  # Enable SSH in the boot process.
  systemd.services.sshd.wantedBy = pkgs.lib.mkForce [ "multi-user.target" ];
  services.openssh.enable = true;
  users.users.root.openssh = import /etc/nixos/secrets/openssh.nix;
  nixpkgs.config.allowUnfree = true;

  ##################################################################################
  # Select internationalisation properties.
  ##################################################################################
  i18n = {
    consoleUseXkbConfig = true;
    consoleFont = if use_retina then "${pkgs.terminus_font}/share/consolefonts/ter-u28n.psf.gz" else "Lat2-Terminus16";
    defaultLocale = "en_US.UTF-8";
  };
  programs.vim.defaultEditor = true;

  ##################################################################################
  # Keyboard layout & editor
  ##################################################################################
  services.xserver = {
    layout = "us";
    xkbVariant = "colemak";
    xkbOptions = "ctrl:nocaps";
  };

  # Time and Location
  time.timeZone = "US/Eastern";

  #  # imports require setting this at the top level
  #  # nixpkgs.config.allowUnfree = true;

  # always make nvidia available
  boot.extraModulePackages = [ pkgs.linuxPackages.nvidia_x11 ];

  # List packages installed in system profile. To search, run:
  environment.systemPackages = with pkgs; [
    # unzip anything
    unar xz zip unzip

    # system statistics
    sysstat
    lsof
    lshw
    pstree
    pciutils

    # set up basic developer tools
    tmux vim neovim neovim-remote mosh git gawk less lesspipe most watch gnused git-lfs bash-completion file finger_bsd diceware
    strace
    killall
    wget
    # make sure you can access the filesystem
    tree

    # set up fancy developer tools
    exa colordiff asciinema tldr ripgrep cloc par fasd httpie fd
    hexyl # read binary
    bat   # read anything
    noti
    prettyping

    # set up nix developer tools
    nix-prefetch-git
    nix-serve
    nix-bash-completions
    nix-info
    nix-index
    nox

    ranger
    # ranger previews
    libcaca   # video
    highlight # code
    atool     # archives
    w3m       # web
    poppler   # PDF
    mediainfo # audio and video

    # Basic utilities
    fd
    ncdu
    ripgrep
    unzip
    zstd

    # networking
    bind
    nmap
    socat
    metasploit
    traceroute
    tcptraceroute
    tcpdump
    nettools
    mbuffer
  ];
}
