{ pkgs, ... }:

{
  # List packages installed in system profile. To search, run:
  environment.systemPackages = with pkgs; [
    #################################################################################
    # Packages for Servers
    #################################################################################

    # build anything
    # missing: clang, ninja, babel, shake?
    gcc coreutils gnumake binutils

    # unzip anything
    unar xz zip unzip

    # system statistics
    sysstat
    lsof
    lshw
    pstree

    # set up sensors
    lm_sensors

    # set up basic developer tools
    tmux vim neovim neovim-remote mosh git gawk less lesspipe most watch gnused git-lfs bash-completion file finger_bsd

    # set up fancy developer tools
    exa colordiff asciinema tldr ripgrep cloc par fasd httpie fd
    cracklib       # word dictionary
    unstable.hexyl # read binary
    unstable.bat   # read anything
    unstable.noti
    unstable.prettyping
    (with unstable; haskell.lib.justStaticExecutables haskellPackages.glirc)
    unstable.powerline-fonts
    unstable.imagemagick

    # set up nix developer tools
    nix-prefetch-git
    nix-serve
    nix-bash-completions
    nix-info
    nix-index

    # email
    neomutt lynx notmuch-mutt
    unstable.protonmail-bridge
    xpdf     # view pdf in terminal via pdftotext
    w3m      # view html in terminal

    # pinnedKernelPackages.bpftrace
    killall
    wget

    # To build things outside of nix
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

    # get basic dev libraries
    openssl.dev openssl.out zlib.dev

    # make sure you can access the filesystem
    ranger tree

    # Basic utilities
    fd
    ncdu
    ripgrep
    unzip
    zstd

    # networking
    nmap
    socat
    metasploit
  ];
}
