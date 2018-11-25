{ lib, pkgs, ... }:

{
  xdg.dataFile = {
    "vim_gmake" = {
      executable = true;
      target = "../bin/vim_gmake";
      text = ''
        #!/usr/bin/env sh
        case "$(uname -o)" in
          "FreeBSD") gmake ;;
          *) make ;;
        esac
      '';
    };
    "vl" = {
      executable = true;
      target = "../bin/vl";
      source = ./vl.sh;
    };

    "vim-plug" = {
      target = "nvim/site/autoload/plug.vim";
      source = builtins.fetchurl {
        url = "https://raw.githubusercontent.com/junegunn/vim-plug/734d9a11b5a6354e6a66e152dee5d311233e033c/plug.vim";
        sha256 = "1rpqfgxrws6298yhaj395czmqa7nlicg5s900vnr4gf84npmr2p6";
      };
    };
  };

  config = {
    enable = true;
    extraPython3Packages = (ps: with ps; [ python-language-server ]);
    viAlias = true;
    vimAlias = true;
    configure = {
      customRC = (import ./customRC.nix { inherit lib; }).customRC;
      # Vam is SLOOOOOW
      # vam = import ./vam.nix { inherit pkgs; };
    };
  };
}
