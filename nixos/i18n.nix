{ pkgs, ... }:

{
  # Select internationalisation properties.
  i18n = {
    #consoleFont = "Lat2-Terminus16";
    #consoleKeyMap = "colemak";
    consoleUseXkbConfig = true;
    inputMethod = {
      enabled = "ibus";
      ibus.engines = with pkgs.ibus-engines; [
        table
        table-others # LaTeX support
        m17n
        uniemoji # ibus 1.5.14 has emoji support : P
      ];
    };
    defaultLocale = "en_US.UTF-8";
  };
}
