{ pkgs, ... }:

{
  xdg.configFile."glirc/config".text = (pkgs.callPackage ./config.nix {}).config;
}
