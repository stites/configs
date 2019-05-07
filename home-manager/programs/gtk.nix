{ pkgs, ... }:
{
  gtk = {
    enable = true;
    gtk3 = {
      extraConfig = {
        gtk-cursor-blink = false;
        gtk-recent-files-limit = 20;
      };
    };
    theme = {
      package = pkgs.numix-gtk-theme;
      name = "Numix";
    };
    iconTheme = {
      package = pkgs.numix-icon-theme;
      name = "Numix";
    };
    font = {
      name = "DejaVu Sans 12";
      package = pkgs.dejavu_fonts;
    };
  };
}
