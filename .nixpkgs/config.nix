{
  allowUnfree = true;
  allowBroken = true;

  # pkgs_ is the original set of packages
  packageOverrides = pkgs_: with pkgs_; {

    # pkgs is your overriden set of packages itself
    all = with pkgs; buildEnv {
      name = "all";
      paths = [
        # filesystem
        silver-searcher
        bashCompletion
        fasd
        gawk

        # development
        git
        gitAndTools.hub
        gitAndTools.git-extras
        emacs
        neovim
        aspell
        aspellDicts.en
        ctags
        gnumake
        (import ./vim/vim.nix)

        # haskell
        cabal-install
        stack

        # systems
        htop
        iftop
        nox
        tmux
        tmuxinator
        mosh
        tree
        watch
        wget
        xz
        traceroute
        unrar
        unzip
        zip

        # extras
        graphviz
        imagemagick
        irssi
        youtube-dl
      ];
    };
  };

  # develEnv = lib.lowPrio (
  #     pkgs.buildEnv {
  #     name = "development-env";
  #     ignoreCollisions = true;
  #     paths = [
  #       automake
  #       clang
  #       cmake
  #       ctags
  #       freeglut
  #       gdb
  #       gcc
  #       gnumake
  #       jdk
  #       llvm
  #       manpages
  #       mesa
  #       pciutils
  #       pkgconfig
  #       python
  #       python34
  #       rustUnstable.rustc
  #       cargo
  #       smartmontools
  #       subversion
  #       swiProlog
  #       xlibs.libX11
  #       zlib
  #     ];
  #     }
  # );
}
