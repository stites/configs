{ pkgs, ... }:
{
  services.keybase.enable = true;
  services.kbfs.enable = true;
  services.kbfs.mountPoint = "keybase"; # default: "keybase". Folder relative to HOME
  services.kbfs.extraFlags = [ ]; # default: [].
  home.packages = [ pkgs.keybase-gui ];
}
