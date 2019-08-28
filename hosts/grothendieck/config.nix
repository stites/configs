{ lib, ... }:
let
  colors = import ../../programs/bash/colors.nix;
  libraryPaths = {
    nixpkgs = "${builtins.getEnv "HOME"}/.nix-profile/lib";
    nixos = concatStringsSep ":" [
      # This is a global graphics parameter which MUST BE SET for nvidia to work.
      # FIXME: find reference
      "/run/opengl-driver/lib"
      "/run/current-system/sw/lib"
    ];
  };
  includePaths = {
    nixpkgs = "${builtins.getEnv "HOME"}/.nix-profile/include";
  };
in
{
  is = {
    workstation = true;
    server = true;
  };
  bash = {
    libraryPath = lib.strings.concatStringsSep ":" [libraryPaths.nixpkgs libraryPaths.nixos];
    includePath = includePaths.nixpkgs;
    prompt = {
      PROMPT_INCL_USER="false";
      PROMPT_USER_COLOR="${colors.WHITE}";
      PROMPT_HOST_COLOR="${colors.GREEN}";
      PROMPT_HOST_COLOR_GIT="";
      PROMPT_SEP_COLOR="${colors.NORMAL}";
      PROMPT_PATH_COLOR="${colors.YELLOW}";
      PROMPT_PATH_COLOR_GIT="${colors.YELLOW}";
      PROMPT_BRANCH_COLOR_GIT="${colors.CYAN}";
      GPU_LIGHTS_ON="nocheck";
    };
    extraConfig = "";
  };
}
