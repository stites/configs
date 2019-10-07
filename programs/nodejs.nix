{ pkgs, ... }:
{
  home.file.".npmrc".text = ''
    prefix = ${builtins.getEnv "HOME"}/.npm-packages
  '';
}
