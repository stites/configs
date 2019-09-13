{ vimUtils, fetchFromGitHub, stdenv }:
{ name, tarball, homepage, path ? ""}:

vimUtils.buildVimPlugin rec {
  inherit name;
  src = (builtins.fetchTarball { url = tarball; }) + path;
  meta = {
    inherit homepage;
    maintainers = [ ];
  };
}

