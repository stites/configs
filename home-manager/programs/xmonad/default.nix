{ termcommand # more of an annotation to indicate that this requires updating a string somewhere
}:

{
  home.file.".xmonad/xmonad.hs" = {
    source = ./xmonad.hs;
    onChange = ''
      echo "Recompiling xmonad"
      $DRY_RUN_CMD xmonad --recompile

      # Attempt to restart xmonad if X is running.
      if [[ -v DISPLAY ]] ; then
        echo "Restarting xmonad"
        $DRY_RUN_CMD xmonad --restart
      fi
    '';
  };
  # xdg.configFile = {
  #   "taffybar/taffybar.hs" = {
  #     source = ./programs/taffybar/taffybar.hs;
  #     onChange = "rm -rf ${homedir}/.cache/taffybar/";
  #   };
  #   "taffybar/taffybar.css" = {
  #     source = ./programs/taffybar/taffybar.css;
  #     onChange = "rm -rf ${homedir}/.cache/taffybar/";
  #   };
  # };

  # services.taffybar = {
  #   enable = host.isNixOS;
  #   package = hpkgs822.taffybar;
  # };

  # services.status-notifier-watcher = {
  #   enable = host.isNixOS;
  #   package = hpkgs822.status-notifier-item;
  # };

}
