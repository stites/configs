{ ... }:

{
  imports = [
    "${builtins.fetchGit { url="https://github.com/rycee/home-manager"; ref="master"; }}/nixos"
  ];

  services.logind.lidSwitch = "hibernate";
}
