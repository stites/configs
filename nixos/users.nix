{
  # Define a user account. Don't forget to set a password with ‘passwd’.
  # users.extraUsers.stites = {
  users.users.stites = {
    isNormalUser = true;
    home = "/home/stites";
    description = "Sam Stites";
    extraGroups = [ "wheel" "disk" "systemd-journal" "docker" "networkmanager" ];
    # uid = 1000;
  };


  # do I need this???
  # home-manager.users.stites = import ./home.nix;
}
