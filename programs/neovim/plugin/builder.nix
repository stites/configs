{ vimUtils, fetchFromGitHub, stdenv }:
{ name, homepage, rev ? null, tarball ? null, path ? ""}:

assert rev != null || tarball != null;
let
  url = if tarball != null then tarball else "${homepage}/archive/${rev}.tar.gz";
in
vimUtils.buildVimPlugin rec {
  inherit name;
  src = (builtins.fetchTarball { inherit url ; }) + path;
  meta = {
    inherit homepage;
    maintainers = [ ];
  };
}

