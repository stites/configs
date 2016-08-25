{
  allowUnfree = true;
  allowBroken = true;
  packageOverrides = pkgs_: with pkgs_; {  # pkgs_ is the original set of packages
    all = with pkgs; buildEnv {  # pkgs is your overriden set of packages itself
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
        ctags
        gnumake
        vim

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

        # extras
        graphviz
        imagemagick
        irssi
        youtube-dl
      ];
    };
  };
}
