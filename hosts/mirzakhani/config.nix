{ lib, ... }:
let
  libraryPaths = {
    nixpkgs = "${builtins.getEnv "HOME"}/.nix-profile/lib";
    nixos = lib.strings.concatStringsSep ":" [
      # This is a global graphics parameter which MUST BE SET for nvidia to work.
      # FIXME: find reference
      "/run/opengl-driver/lib"
      "/run/current-system/sw/lib"
    ];
  };
  includePaths = {
    nixpkgs = "${builtins.getEnv "HOME"}/.nix-profile/include";
  };

  libraryPath = lib.strings.concatStringsSep ":" [libraryPaths.nixpkgs "/usr/local/lib/" "/usr/lib/x86_64-linux-gnu/" "/lib/x86_64-linux-gnu/"];
  includePath = lib.strings.concatStringsSep ":" [includePaths.nixpkgs "/usr/local/include/" "/usr/include/"];
in
{
  is = {
    server = true;
    workstation = true;
  };
  bash = {
    inherit libraryPath includePath;
    prompt = {
      PROMPT_INCL_USER="false";
      PROMPT_USER_COLOR="${colors.WHITE}";
      PROMPT_HOST_COLOR="${colors.VIOLET}";
      PROMPT_HOST_COLOR_GIT="";
      PROMPT_SEP_COLOR="${colors.NORMAL}";
      PROMPT_PATH_COLOR="${colors.BLUE}";
      PROMPT_PATH_COLOR_GIT="${colors.YELLOW}";
      PROMPT_BRANCH_COLOR_GIT="${colors.CYAN}";
      GPU_LIGHTS_ON="check";
    };
    extraConfig = ''
      source "$HOME/.nix-profile/etc/profile.d/hm-session-vars.sh"
      safe_path_add "/usr/local/cuda/bin"
      function GPULogoOff {
        local GPU_NUM="$1"
        GPU_BRIGHTNESS="$(nvidia-settings --query [gpu:$GPU_NUM]/GPULogoBrightness --terse 2>/dev/null)"
        if [ "$GPU_BRIGHTNESS" != "0" ] && [ "$GPU_BRIGHTNESS" != "" ]; then
          nvidia-settings --assign [gpu:$GPU_NUM]/GPULogoBrightness=0 1> /dev/null
        fi
      }
      if command -v nvidia-settings &> /dev/null ; then
        GPULogoOff 0
        GPULogoOff 1
        GPULogoOff 2
        GPULogoOff 3
      fi
    '';
  };
}
