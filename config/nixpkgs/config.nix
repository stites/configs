{
  allowUnfree = true;
  # allowBroken = true;

  # firefox = {
  #   enableAdobeFlash = true;
  #   enableGoogleTalkPlugin = true;
  # };

  packageOverrides = pkgs_: with pkgs_; {

    rubber151 =
      python2Packages.buildPythonApplication rec {
        name = "rubber-${version}";
        version = "1.5.1";
      
      src = fetchurl {
          url = "https://launchpad.net/rubber/trunk/${version}/+download/${name}.tar.gz";
          sha256 = "1d7hq19vpb3l31grldbxg8lx1qdd18f5f3gqw96q0lhf58agcjl2";
        };
      
        propagatedBuildInputs = [ texinfo ];
      
        # I couldn't figure out how to pass the proper parameter to disable pdf generation, so we
        # use sed to change the default
      preBuild = ''
          sed -i -r 's/pdf\s+= True/pdf = False/g' setup.py
        '';
      
        # the check scripts forces python2. If we need to use python3 at some point, we should use
        # the correct python
      checkPhase = ''
          sed -i 's|python=python2|python=${python2Packages.python.interpreter}|' tests/run.sh
          cd tests && ${stdenv.shell} run.sh
        '';
      
      meta = with stdenv.lib; {
          description = "Wrapper for LaTeX and friends";
      longDescription = ''
            Rubber is a program whose purpose is to handle all tasks related
            to the compilation of LaTeX documents.  This includes compiling
            the document itself, of course, enough times so that all
            references are defined, and running BibTeX to manage
            bibliographic references.  Automatic execution of dvips to
            produce PostScript documents is also included, as well as usage
            of pdfLaTeX to produce PDF documents.
          '';
          license = licenses.gpl2Plus;
          homepage = https://launchpad.net/rubber;
          maintainers = with maintainers; [ ttuegel peterhoeg ];
          platforms = platforms.unix;
        };
      };
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
        bash-completion
        bashInteractive

        # dev tools
        vim
        neovim
        gotty
        graphviz
        sqlite
        sqliteman
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
  };
}
