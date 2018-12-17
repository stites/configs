{ pkgs, lib, ... }:

let
  hostname = lib.strings.removeSuffix "\n" (builtins.readFile "/etc/hostname");
  colors = import ./programs/bash/colors.nix;
  isGrothendieck = hostname == "grothendieck";
  isMirzakhani = hostname == "mirzakhani";
  grothendieck.bash = {
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
  mirzakhani.bash = {
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
      export PATH="/usr/local/cuda/bin:$PATH"
      # export PATH="/home/stites/.local/bin:$PATH"
      export LD_LIBRARY_PATH="/usr/local/cuda/lib64:$LD_LIBRARY_PATH"
      # eval "$(hub alias -s)"

      export PYENV_ROOT="$HOME/.pyenv"
      export PATH="$PYENV_ROOT/bin:$PATH"
      eval "$(pyenv init -)"
      eval "$(pyenv virtualenv-init -)"

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
in
{
  inherit hostname;
  isNixOS = isGrothendieck;
  bash = if isGrothendieck then grothendieck.bash else mirzakhani.bash;
}
