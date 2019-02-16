{ pkgs, ... }:

{
  environment.systemPackages = [
    (pkgs.haskellPackages.ghcWithPackages (self: with self; [
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
      # accelerate              # An embedded language for accelerated array processing
      # accelerate-io           # Read and write Accelerate arrays in various formats (vector, array, bytestring, repa, bmp)
      # lens-accelerate         # Instances to mix lens with accelerate
      # mwc-random-accelerate   # Generate Accelerate arrays filled with high quality pseudorandom numbers
      # # accelerate-blas         # BROKEN: also builds cublas. Numeric Linear Algebra in Accelerate
      # # accelerate-llvm         # BROKEN: requires llvm == 7.0.*. Accelerate backend component generating LLVM IR
      # # accelerate-llvm-native  # BROKEN: requires llvm == 7.0.*. Accelerate backend for multicore CPUs
      # # accelerate-llvm-ptx     # Accelerate backend for NVIDIA GPUs
      backprop                # AD library
      cassava                 # CSV encode/decode
      dsp                     # signal processing
      Frames                  # frames library
      fgl                     # Martin Erwig's Functional Graph Library (efficient graph algorithms & manuipulation)
      hmatrix                 # LAPACK library
      linear                  # types and combinators for linear algebra on free vector spaces
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
