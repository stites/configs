{ pkgs, ... }:
{
  services.keybase.enable = true;
  services.kbfs.enable = true;
  services.kbfs.mountPoint = "keybase"; # default: "keybase". Folder relative to HOME
  services.kbfs.extraFlags = [ ]; # default: [].
  home.packages = [ pkgs.keybase-gui ];
  xdg.dataFile."keybase-newbox" = {
    executable = true;
    target = "../bin/keybase-newbox";
    text = ''
      #!/usr/bin/env bash
      echo ''${HOSTNAME}-$(date +%s)
    '';
  };

}
