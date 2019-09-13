let
  pkgs = import <nixpkgs> {};
  functions = pkgs.callPackage ./functions.nix {};
  all-deps = p: with functions; flatten-plugins (calltree p);

  test-suite = f: {
      single     = (f ./tests/single.nix);
      simple-dep = f ./tests/simple-dep.nix;
      tree       = f ./tests/tree;
      rc-merge   = f ./tests/rc-merge;
      pkg-mismatch = f ./tests/pkg-mismatch;
      diamond      = f ./tests/diamond;
    };
in
with functions;
{
  functions = {
    inherit (functions) compile mergeplugs countAll calltree flatten-plugins smoosh smooshAll validPluginFile;
    inherit all-deps;
  };
  tests = {
    calltree = test-suite calltree;
    all-deps = test-suite all-deps;
    smoosh = test-suite smoosh;
    smooshAll = smooshAll [ ./tests/single.nix ./tests/simple-dep.nix ./tests/tree ./tests/rc-merge ./tests/diamond ];
    smooshAllSorted = smooshAllSorted [ ./tests/single.nix ./tests/simple-dep.nix ./tests/tree ./tests/rc-merge ./tests/diamond ];
  };

  myplugins = pkgs.callPackage ../myplugins {};
}
