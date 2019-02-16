{
  services.gnome3.gnome-keyring.enable = true;

  programs.gnupg = {
    agent = {
      enable = true;
      enableSSHSupport = true;
      enableExtraSocket = false;
      enableBrowserSocket = false;
    };
    dirmngr.enable = true;
  };

  security.sudo = {
    enable = true;
    wheelNeedsPassword = false;
  };

  # FIXME: remove stites when cachix can do group checks
  nix = let superusers = [ "@wheel" "root" "stites" ]; in {
    allowedUsers = superusers;
    trustedUsers = superusers;
  };
}
