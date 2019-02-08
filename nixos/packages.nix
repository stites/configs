{ pinnedKernelPackages }: { pkgs, config, ... }:

{
  nixpkgs.config.firefox = {
    enableGoogleTalkPlugin = true;
    enabledAdobeFlash = true;
    enabledAdobeFlashDRM = true;
    jre = false;
    icedtea = true;
  };

  # Enable CUPS to print documents.
  services.printing = {
    enable = true;
    browsing = true;
    defaultShared = true;
    tempDir = "/tmp";
    webInterface = true;
    drivers = [
      pkgs.gutenprint # drivers for most printers (use gutenprintBin for binary-only drivers)
      # pkgs.hplip # HP printers
      pkgs.hplipWithPlugin # HP printers with proprietary plugin run `nix run nixpkgs.hplipWithPlugin -c sudo hp-setup` to add the printer.
      # pkgs.splix # printers supporting SPL (Samsung Printer Language)
      # pkgsc.brlaser # Brother printers
    ];
  };
  # Use Avahi to find network printers as well.
  services.avahi = {
    enable = true; # please harden this configuration
    nssmdns = true;
  };

  services.logind.lidSwitch = "hibernate";

  # join the interplanetary file system
  services.ipfs = {
    enable = true;
    enableGC = true;
    autoMount = true;
  };

  #Turn on docker
  virtualisation.docker.enable = true;

  #JournalD settings
  services.journald.extraConfig = "Storage=persistent";

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  services.postgresql = {
    enable = true;
    package = pkgs.postgresql100;
    enableTCPIP = true;
    authentication = pkgs.lib.mkOverride 10 ''
      # Generated file; do not edit!
      # TYPE   DATABASE  USER    ADDRESS        METHOD
      local    all       all                    trust
      host     all       all     127.0.0.1/32   trust
      host     all       all     ::1/128        trust
    '';
  };

  # List packages installed in system profile. To search, run:
  environment.systemPackages = with pkgs; [
    cracklib gcc coreutils pstree tree killall gnumake binutils wget pinnedKernelPackages.bpftrace

    unar
    unstable.hexyl

    # optimize power
    powertop xorg.xbacklight

    htop git micro rofi salt
    unstable.gitAndTools.git-annex
    unstable.gitAndTools.git-annex-metadata-gui

    # set up sensors
    lm_sensors

    # set up basic developer tools
    tmux vim neovim neovim-remote

    # In order to build things outside of nix
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

    # get required system-level GUI apps
    dmenu albert rxvt_unicode-with-plugins networkmanagerapplet
    networkmanager_dmenu

    # web-based GUI apps
    chromium firefox # slack signal-desktop

    # multimonitor utils
    arandr autorandr

    # make sure you can access the filesystem 
    ranger xfce.thunar

    # get basic dev libraries
    openssl.dev openssl.out zlib.dev

    # in case you forget something on that other operating system you are dual booting
    lvm2 cryptsetup

    # wireguard wireguard-tools

    # Basic utilities
    fd
    ncdu
    ripgrep
    unzip
    zstd
    pastebinit

    # networking
    nmap
    metasploit

    (haskellPackages.ghcWithPackages (self: with self; [
        # https://github.com/Gabriel439/post-rfc/blob/master/sotu.md
        async
        bytestring
        cabal-install
        criterion
        comonad # use this more??
        containers
        contravariant # use this more??

        deepseq
        fast-logger # use this!
        free # use this more??
        JuicyPixels
        lens
        lens-aeson
        machines
        mtl
        mwc-random
        streaming
        stm
        text
        transformers
        unagi-chan
        vector
        unordered-containers

        # Numeric programming
        accelerate              # An embedded language for accelerated array processing
        accelerate-io           # Read and write Accelerate arrays in various formats (vector, array, bytestring, repa, bmp)
        # accelerate-blas         # BROKEN: also builds cublas. Numeric Linear Algebra in Accelerate
        # accelerate-llvm         # BROKEN: requires llvm == 7.0.*. Accelerate backend component generating LLVM IR
        # accelerate-llvm-native  # BROKEN: requires llvm == 7.0.*. Accelerate backend for multicore CPUs
        # accelerate-llvm-ptx     # Accelerate backend for NVIDIA GPUs
        backprop                # AD library
        cassava                 # CSV encode/decode
        dsp                     # signal processing
        Frames                  # frames library
        fgl                     # Martin Erwig's Functional Graph Library (efficient graph algorithms & manuipulation)
        hmatrix                 # LAPACK library
        lens-accelerate         # Instances to mix lens with accelerate
        linear                  # types and combinators for linear algebra on free vector spaces
        mwc-random-accelerate   # Generate Accelerate arrays filled with high quality pseudorandom numbers
        statistics              # basic statistics
        vector-space            # vector & affine spaces. Also defines a type of infinite towers of generalized derivatives.

        # Plotting
        gnuplot             # haskell bindings to gnuplot.
        # plot              # BROKEN: gtk version requires base <4.12. a haskell-based gnuplot replacement (last update 12/2017 as of 01/2019).
        diagrams            # full-featured framework and EDSL for vector graphics. Includes -core, -contrib, -lib, -svg
        # diagrams-cairo    # full-featured backend for rendering diagrams using the cairo rendering engine.
        # diagrams-gtk      # BROKEN: gtk version requires base <4.12. Backend for rendering diagrams directly to GTK windows.
        # diagrams-graphviz # BROKEN: requires fgl <5.7. Graph layout and drawing with GrahpViz and diagrams

        # System libraries
        directory # actions on directories
        filepath  # how to work with filepaths
        turtle    # shell-scripting in haskell

        # Networking
        req   # cool, new, hip, lightweight requests library making use of datakinds.
        wreq  # tried-and-true library that depends on lens, so you will import the entire world.

        # Parsing:
        attoparsec # very fast, but only pasing text (so no token streams). Everything backtracks on failure.
        megaparsec # fork of parsec, claiming to be better (also more actively maintained than parsec).
        trifecta # focuses on error reporting, not sure of how it performs compared to others.

        # Serialization
        aeson                      # foundational json serialization library
        cborg serialise cborg-json # faster than binary, user-friendly parsing. JSON-like RFC spec.
        flat                       # speed-optimized, uses bit- instead of byte- level encoding.

        # X11 libaries
        xmonad
        xmonad-contrib
        xmonad-extras
        # taffybar # broken build on status-notifier-item

        # all of shake that builds
        shake
        shake-c
        shake-cabal
        shake-ccjs
        shake-elm
        shake-ext
        shake-google-closure-compiler
        shake-language-c
        shake-literate
    ]))
  ];
}

