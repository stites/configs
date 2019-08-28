{ pkgs, lib, ... }:
let
  hostname = lib.strings.removeSuffix "\n" (builtins.readFile "/etc/hostname");
  hostconfig = builtins.toPath (builtins.toString ./. + "/${hostname}/config.nix");
  host = pkgs.callPackage hostconfig {};
in
rec {
  inherit (host) hostname bash;
  is = host.is // { NixOS = (builtins.tryEval (import <nixos> {})).success; };
  homedir = builtins.getEnv "HOME";
  confroot = "${homedir}/git/configs/";
}
