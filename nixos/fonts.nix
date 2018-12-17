{ config, pkgs, ... }:

{
  fonts = {
    fontconfig.enable = true;
    enableFontDir = true;
    enableGhostscriptFonts = true;
    fonts = with pkgs; [
      fira
      fira-code
      fira-mono
      font-droid
      inconsolata
      ubuntu_font_family
      nerdfonts
      libre-caslon
      libertinus
    ];
  };
}
