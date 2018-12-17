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

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.stites = {
    isNormalUser = true;
    home = "/home/stites";
    description = "Sam Stites";
    extraGroups = [ "wheel" "networkmanager" ];
  };

  security.sudo = {
    enable = true;
    wheelNeedsPassword = false;
  };

  nix = let superusers = [ "@wheel" "root" ]; in {
    allowedUsers = superusers;
    trustedUsers = superusers;
  };
}
