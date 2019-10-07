{ pkgs, ... }:
{
  xdg.dataFile = {
    "home-manager-dev" = {
      executable = true;
      target = "../bin/home-manager-dev";
      text = ''
        #!/usr/bin/env bash
        home-manager -I home-manager=${builtins.getEnv "HOME"}/git/home-manager $@
      '';
    };
    "use_nix" = {
      executable = true;
      target = "../bin/use_nix";
      source = ./use_nix.sh;
    };
  };
}
