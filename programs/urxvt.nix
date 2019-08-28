{ pkgs, ... }:

{
  xdg.configFile."urxvt/color-themes" = {
    recursive = true;
    source = pkgs.fetchFromGitHub {
      owner = "felixr";
      repo = "urxvt-color-themes";
      rev = "56589a340f76c26486d8575fa639834aa7c248ea";
      sha256 = "1jhk606ayd1qkphm63da7g5wy4y68n1bjdqjwrjy37nxsri01hvy";
    };
  };

  programs.urxvt = {
    enable = true;
    package = pkgs.rxvt_unicode-with-plugins;
    fonts = [ "xft:FuraCode Nerd Font Mono:pixelsize=20" ];
    scroll.bar.enable = false;
    extraConfig = {
      # termName must be screen otherwise vim colorschemes will break in subtle ways
      "termName" = "rxvt-256color";
      "reverseVideo" = "True";
      "boldFont" = "xft:FuraCode Nerd Font Mono:bold:pixelsize=20";
      # Enable Shift-PageUp/Down in screen
      "secondaryScroll" = "true";
      # letterSpace: -1
      "urlLauncher" = "firefox";
      "matcher.button" = "1";
      "scrollstyle" = "plain";
      "URxvt*fading" = "30";
      "saveLines" = "4055";
      "iso14755" = "False"; # unicode?
      "iso14755_52" = "False"; # unicode?

      "perl-ext-common" = "default,matcher,font-size,color-themes";
      "color-themes.state-file" = "~/.urxvt-theme";
      "color-themes.autosave" = "1";
      "bell-command" = ''notify-send "Beep, Beep"'';
      "urgentOnBell" = "true";

      "color-themes.themedir" =  "~/git/configs/tools/urxvt/urxvt-color-themes/";
      "color-themes.preprocessor" = "/home/stites/.nix-profile/bin/cpp";
    };

    keybindings = {
      "M-C-l" =  "perl:color-themes:load-state";
      "M-C-s" =  "perl:color-themes:save-state";
      "M-escape" = "perl:keyboard-select:activate";
      "C-Up" =     "font-size:increase";
      "C-Down" =   "font-size:decrease";
      "C-S-Up" =   "font-size:incglobal";
      "C-S-Down" = "font-size:decglobal";
      "C-equal" =  "font-size:reset";
      "C-slash" =  "font-size:show";

      "Shift-Control-Left" =   "perl:color-themes:prev";
      "Shift-Control-Right" =  "perl:color-themes:next";

      "Shift-Control-V" = "eval:paste_clipboard";
      "Shift-Control-C" = "eval:selection_to_clipboard";
      "Shift-Control-Up" = "perl:font-size:increase";
      "Shift-Control-Down" = "perl:font-size:decrease";

      "Control-Up" =    "\\033[1;5A";
      "Control-K" =     "\\033[1;5A";
      "Control-Down" =  "\\033[1;5B";
      "Control-J" =     "\\033[1;5B";
      "Control-Left" =  "\\033[1;5D";
      "Control-H" =     "\\033[1;5D";
      "Control-Right" = "\\033[1;5C";
      "Control-L" =     "\\033[1;5C";
    };
  };
}
