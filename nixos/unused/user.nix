# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  

   users.extraUsers.noah = {
     isNormalUser = true;
     extraGroups = [ "wheel" "disk" "systemd-journal" "docker" "networkmanager" ];
     uid = 1000;
   };

  home-manager.users.noah = import ./home.nix;


}

