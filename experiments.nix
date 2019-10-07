{ pkgs, lib, ... }:
{
  programs.starship = {
    enable = true;
    enableBashIntegration = true;
    settings = {
      "add_newline" = false;
    };
  };
}
