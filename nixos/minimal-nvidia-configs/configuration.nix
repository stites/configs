# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
#     #"${builtins.fetchTarball https://github.com/rycee/home-manager/archive/master.tar.gz}/nixos"
#     "${builtins.fetchGit { url="https://github.com/rycee/home-manager"; ref="master"; }}/nixos"
     ./fan.nix
     ./system76
     ./x11.nix
     ./boot.nix
     ./services.nix
     ./user.nix
     ./networking.nix
     ./system-packages.nix
    ];

  nixpkgs.config.allowUnfree = true;

  
  # Set your time zone.
   time.timeZone = "America/New_York";


  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;


  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "18.09"; # Did you read the comment?
  

  nix.nixPath = [ 
    "nixpkgs=/root/nixpkgs" 
    "nixos-config=/etc/nixos/configuration.nix"
  ]; 

}
