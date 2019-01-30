{ pinnedKernelPackages }: { pkgs, lib, ... }:
let
  fetchFromGitHub = pkgs.fetchFromGitHub;
  fetchFromRawGitHub = (attrs: builtins.fetchurl {
    url = "https://raw.githubusercontent.com/${attrs.owner}/${attrs.repo}/${attrs.rev}/${attrs.path}";
    sha256 = attrs.sha256;
  });
in
{
  boot.extraModulePackages = [ pinnedKernelPackages.wireguard ];

  # Enable various networking daemons
  services.openssh.enable = true;
  services.sshguard.enable = false;
  services.dnscrypt-proxy.enable = false;

  environment.etc."hosts".text = (builtins.concatStringsSep "\n" [
    "10.0.6.154 genbu"
    "10.0.6.124 grothendieck"
    "10.0.6.132 mirzakhani"
    (builtins.readFile (fetchFromRawGitHub {
      owner = "StevenBlack";
      repo = "hosts";
      rev = "v2.1.19";
      path = "hosts";
      sha256 = "094z1q76yyncmxz18wllg2qai9vp6sw4qixf34gb4mfjsq2fz1c5";
    }))
  ]);

  networking = {
    hostName = "grothendieck";
    nameservers = ["1.1.1.1" "8.8.8.8"];

    firewall = {
      enable = false;
      allowPing = true;
      pingLimit = "--limit 60/minute --limit-burst 5";
      allowedTCPPorts      = [ 22 1337 655 ];
      allowedUDPPortRanges = [
        { from = 8000;  to = 8010; } # web servers
        { from = 60000; to = 60050; } # mosh
        { from = 655;   to = 656; } # tinc
        # 655 # tinc
      ];
      trustedInterfaces = [ "tun" ];
    };

    networkmanager.enable = true;
  };
}
