{
  allowUnfree = true;
  # allowBroken = true;

  # firefox = {
  #   enableAdobeFlash = true;
  #   enableGoogleTalkPlugin = true;
  # };

  packageOverrides = pkgs_: with pkgs_; {
    noti = buildGoPackage rec {
      name = "noti-${version}";
      version = "3.1.0";
      owner = "variadico";
      projectPath = "github.com/${owner}/noti";
      src = fetchFromGitHub {
        inherit owner;
        repo = "noti";
        rev = "${version}";
        sha256 = "1chsqfqk0pnhx5k2nr4c16cpb8m6zv69l1jvv4v4903zgfzcm823";
        fetchSubmodules = true;
      };
      goPackagePath = "github.com/${owner}/noti";
      preBuild = '' buildFlagsArray+=("-ldflags" "-X ${goPackagePath}/internal/command.Version=${version}") '';
      postInstall = ''
        mkdir -p $out/share/man/man{1,5}/
        cp $src/docs/man/noti.1      $out/share/man/man1/
        cp $src/docs/man/noti.yaml.5 $out/share/man/man5/
      '';
      meta = with stdenv.lib; {
        description = "Monitor a process and trigger a notification.";
        longDescription = ''
          Monitor a process and trigger a notification.

          Never sit and wait for some long-running process to finish. Noti can alert you when it's done. You can receive messages on your computer or phone.
        '';
        homepage = https://github.com/variadico/noti;
        license = licenses.mit;
        # maintainers = [ maintainers.stites ];
        platforms = platforms.all;
      };
    };

    prettyping = stdenv.mkDerivation rec {
      name = "prettyping-${version}";
      version = "1.0.1";

      src = fetchgit {
        url = "https://github.com/denilsonsa/prettyping";
        rev = "e8d7538b8742b27cffe28e9dfe13d1d1a12288e3";
        sha256 = "05vfaq9y52z40245j47yjk1xaiwrazv15sgjq64w91dfyahjffxf";
        fetchSubmodules = false;
      };

      installPhase = ''
        mkdir -p $out/bin
        chmod +x $src/prettyping
        install $src/prettyping $out/bin/
      '';

      meta = with stdenv.lib; {
        description = "A wrapper around the standard ping tool, making the output prettier, more colorful, more compact, and easier to read.";
        longDescription = ''
          `prettyping` runs the standard ping in background and parses its output, showing ping responses in a graphical way at the terminal (by using colors and Unicode characters). Don’t have support for UTF-8 in your terminal? No problem, you can disable it and use standard ASCII characters instead. Don’t have support for colors? No problem, you can also disable them.
        '';
        homepage = http://denilson.sa.nom.br/prettyping/;
        license = licenses.mit;
        # maintainers = [ maintainers.stites ];
        platforms = platforms.all;
      };
    };

    tmux-bundled = import ./tmux-bundled (with pkgs; {
      inherit makeWrapper symlinkJoin writeText tmux git;
    });

    all = with pkgs; let exe = haskell.lib.justStaticExecutables; in buildEnv {
      name = "all";
      paths = [
        # window manager (maybe desktop manager)
        taffybar
        haskellPackages.xmobar
        haskellPackages.xmonad
        haskellPackages.xmonad-contrib

        # system-level GUI deps
        albert
        signal-desktop
        firefox-devedition-bin
        gitter
        slack
        # mendeley # broken

        # keybase     # <<< needs some more coordination
        # keybase-gui # <<< needs some more coordination

        # system level deps
        noti
        prettyping
        prettyping
        bash-completion
        bashInteractive

        #tmuxinator
        ## tmux-bundled
        ## tmuxPlugins.battery
        ## tmuxPlugins.continuum
        ## tmuxPlugins.cpu
        ## tmuxPlugins.fzf-tmux-url
        ## tmuxPlugins.resurrect
        ## tmuxPlugins.sensible

        # dev tools
        vagrant
        vim
        neovim
        gotty
        graphviz
        sqlite
        sqliteman

        # elm
        elmPackages.elm
        elmPackages.elm-format

        # git stuff
        git

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

        # yubico, unexplored tools
        yubico-piv-tool
        yubikey-manager
        yubikey-personalization

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

    golangEnv = buildEnv {
      name = "golangEnv";
      paths = [ dep2nix go2nix go ];
    };

    luatorch-env = buildEnv {
      name = "luatorch-env";
      paths = [
        torchPackages.torch
        # torchPackages.sys
        torchPackages.trepl
        # torchPackages.nn
        # torchPackages.nngraph
        # torchPackages.optim
        # torchPackages.cwrap
        # torchPackages.dok
        # torchPackages.luafilesystem
        # torchPackages.gnuplot
        # torchPackages.graph
        torchPackages.luarocks
        # torchPackages.paths
        # torchPackages.penlight
      ];
    };
  };
}
