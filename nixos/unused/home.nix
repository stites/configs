{ pkgs, ... }:

{

  imports = [
	./git.nix
#	./firefox.nix
#	./i3.nix
];


#  programs.firefox = {
#      enableGoogleTalk = true;
#      enableIcedTea = true;
#      enable = true;
#      firefox.p
#  };


  home.packages = (import ./packages.nix pkgs) ++ (import ./python.nix pkgs);

  nixpkgs.config.allowUnfree = true;
  
  programs.home-manager = {
    enable = true;
  };
}
