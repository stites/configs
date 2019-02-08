{ lib, pkgs }:
let
  fn = (pkgs.callPackage ./functions.nix { });
  no-nix-shell = fn.no-nix-shell;
  mkFunction = fn.mkFunction;
in

{
  functions = ''
    function nix-py3-install {
      nix-env -f "<nixpkgs>" -iA "python36Packages.$1"
    }

    function nix-py3-search {
      nix-env -f "<nixpkgs>" -qaP -A python36Packages -s "$1"
    }
    function nix-hs-install {
      nix-env -f "<nixpkgs>" -iA "haskellPackages.$1"
    }

    function nix-hs-search {
      nix-env -f "<nixpkgs>" -qaP -A haskellPackages | grep -i "$1"
    }
  '';

  initConfig = 
    # only applies to nixos-specific things
    (if ! (builtins.tryEval (import <nixos> {})).success then "" else no-nix-shell ''
      systemctl status display-manager.service &> /dev/null
      ret=$?

      # optionally boot up X when logging into nixos
      if [ $ret -ne 0 ]; then
        printf "Non-root user %s without display. Boot X? ([y]|n) " "$(whoami)"
        read -r bootx
        case "$bootx" in
          ""|"y"|"Y")
            systemctl start display-manager.service
            ;;
        esac
      fi

      # neofetch when on nixos and _not in tmux_
      if [ -z "$\{TMUX_PANE:-}" ] && command -v neofetch &> /dev/null; then
        neofetch
      fi
    '');
}
