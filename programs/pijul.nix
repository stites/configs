{ pkgs, ... }:
{
  home.packages = [ pkgs.pijul ];
  xdg.dataFile."pi" = {
    executable = true;
    target = "../bin/pi";
    text = ''
      #!/usr/bin/env bash
      ${pkgs.pijul}/bin/pijul
    '';
  };
}
