{ pkgs, ... }:
let
  functions = pkgs.callPackage ./functions.nix {};
in
{
  compile = functions.smoosh;
  compileAll = functions.smooshAllSorted;
}

