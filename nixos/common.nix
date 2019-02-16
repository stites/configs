{ pkgs, lib, config, ... }:

let
  fetchFromGitHub = pkgs.fetchFromGitHub;
  host = import ./host.nix;
in
{
  ##################################################################################
  # Select internationalisation properties.
  ##################################################################################
  i18n = {
    consoleUseXkbConfig = true;
    inputMethod = {
      enabled = "ibus";
      ibus.engines = with pkgs.ibus-engines; [
        table
        table-others # LaTeX support
        m17n
        uniemoji # ibus 1.5.14 has emoji support : P
      ];
    };
    defaultLocale = "en_US.UTF-8";
  };

  ##################################################################################
  # Keyboard layout
  ##################################################################################
  services.xserver = {
    layout = "us";
    xkbVariant = "colemak";
    xkbOptions = "ctrl:nocaps";
  };

  ##################################################################################
  # Users
  ##################################################################################

  # do I need this???
  # home-manager.users.stites = import ./home.nix;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  # TODO: what is the difference between users.users and users.extraUsers?
  # TODO: add passwords to hosts.secrets?
  users.users.stites = {
    isNormalUser = true;
    home = "/home/stites";
    description = "Sam Stites";
    extraGroups = [ "wheel" "disk" "systemd-journal" "docker" "networkmanager" ];
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMcW8YuVcR+3Yzv57SvRiL8agZ2FImDOgnkqVocHDICW stites@grothendieck"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKRm9DkTTECY8Tv9yVEZahHCT7xhN9cQTRdDrVjh/0Cg stites@erdos"
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCfW8VuZ0R3/EMWd3aJxjBtlOawr1TEDGKM8oAjraC1h5xFl+gnt9wrBhYpBXbFuec2YyNQcofE8/9tR/tdWloiQrxX4kYH1xe0ujdu7520pjaNnvieLj0P77VzzTvefsIi4po8Jqhsv0M7j0GoWG0yCCPLsH75xyTNkPMVJKg7+J5L8uMbU6BEjnU50f+rRzCLQ6nhV89gYDZwCjEXNr/j8h+EAIAfBM1r4n6/FmjLRPUvAsEKyUKhDv46rQsXqQZnz3tCqNXFhuMlKVb2OZNr5guX/3fJP4JOBTrkrqjHw5iP87EC/crqKcJM7Wc6KzNVsqIlfX/D4vRWq2ip0eTD stites@hesse"
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDXdVmPMaW41d6TmmdHE2IxbI9tBXCDD22dqlwUohOyWnoj5ky8nt23xOBl46PaXNev5O9L/bRlx0vDpWeU5jn+daTkgDU7P9flBmLMMw3srGW7yMDIbKKsT1OutUf0SnjXJDBnvbAS92GVa2Onfd84yvDk7fmHDY/UDIRyhM/EEnPcVnPl6F3vNf2oLNLlBrNcHUlpbHtdEWKmsHJohMT4dXJEH+BfFJDECVLWcwAmLljfQ5pnLBpPVXBcIBCVWqxbj3ZVqddbkodRABD2RCKzic2C7RYI8YNng85ZmotQLkwIO1L6Fyg/quHO0jn6/t7Wyu/4dwXF7hqfyMbfb3fT4any3vwB5SSiPR1447pBeHjx4QluaipFnhpY99Vnj1yNhsqwIUdfOXxpuZoINJWVKc281Py659eQQazNRkGR9Swqpk8KFqkWJhTdIKM/HsW/F+5aTZ3vtLhqN5jtmVaJRXS9GcHC5oEsa3UVBTJyTFjrxKW3LS7axXzsMridSf5++qgcE1WYyXbAxebfO1i65wm1pxhLiy9qOkluyxXTD+c6ppXTo36M6VKAHwwAyFGufvFFYLOL7+xiLr294M9aq/3FCX/EV8ESPibY3XlVd0cEo4HD/w9MdrRdCE+1T8xR+Q4GEMGiXw6nzkfFUvZMjDGl8RH8XsNmSEj0TnuEWQ== sam@mirzakhani"
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCgOfm4y6kn/N+VamNFRraNkkPT6I3/oILGn0ixQlX7w448T1G/8S09kVeaEUXl/RGfmDM+QygQH/I0IHA2ZX22J2XEPrrMa1BoIKDN9ZgpCrvqnA3or6NOo206JdM0+OgvPjmoHPPT2bQWS6hlVxwyAFCiXTPhOScRyKMg1oO6tBMEIsnaPLLc7+WCIYlZV/uqWCXg0c6hjlMwOr9vipAs1PW9pptyuUlb+bay/Wq6G4uGMJfrJOiZ6jq8+rlKPqS6RDfrhJRig5yfVJSkAwxikHv8GEa3CUrE5oVyuMe1/mXbZqXMitg6q/lWjLNqwSUPsbGAoradLaZf9/zYqBU/ stites@genbu"
    ];
  };

  ##################################################################################
  # Cachix and binaries
  ##################################################################################

  # FIXME: remove stites when cachix can do group checks
  nix = let superusers = [ "@wheel" "root" "stites" ]; in {
    allowedUsers = superusers;
    trustedUsers = superusers;
    binaryCaches = [
      "https://cache.nixos.org/"
      "https://nix-linter.cachix.org"
      "https://hie-nix.cachix.org"
    ];
    binaryCachePublicKeys = [
      "nix-linter.cachix.org-1:BdTne5LEHQfIoJh4RsoVdgvqfObpyHO5L0SCjXFShlE="
      "hie-nix.cachix.org-1:EjBSHzF6VmDnzqlldGXbi0RM3HdjfTU3yDRi9Pd0jTY="
    ];

    # POTENTIALLY USEFUL, BUT DON'T DO IT BECAUSE I AM NOT READY
    # nixPath = [
    #   "nixpkgs=/root/nixpkgs"
    #   "nixos-config=/etc/nixos/configuration.nix"
    # ];
  };

  ##################################################################################
  # Time and Location
  ##################################################################################
  time.timeZone = "US/Eastern";
  services.geoclue2.enable = false;
  services.localtime.enable = false;
  services.tlp.enable = true;

  ##################################################################################
  # Networking
  ##################################################################################
  # Enable various networking daemons
  services.openssh.enable = true;
  services.sshguard.enable = false;
  services.dnscrypt-proxy.enable = false;

  environment.etc."hosts".text = (builtins.concatStringsSep "\n" [
    "10.0.6.154 genbu"
    "10.0.6.124 grothendieck"
    "10.0.6.132 mirzakhani"
    "10.0.6.89  erdos"
    (builtins.readFile ((fetchFromGitHub {
      owner = "StevenBlack";
      repo = "hosts";
      rev = "v2.3.1";
      sha256 = "1swcpcl2dvisg2ih2yhifp5nzms6a2h0q0a7g9zp0lnxlv8sszbl";
    }) + "/hosts"))
  ]);

  networking = {
    hostName = host.name;
    nameservers = ["1.1.1.1" "8.8.8.8"]; # cloudflare (encrypted?), then google

    firewall = {
      enable = false;
      allowPing = true;
      pingLimit = "--limit 60/minute --limit-burst 5";
      allowedTCPPorts      = [ 22 1337 655 ];
      allowedUDPPortRanges = [
        { from = 8000;  to = 8010; }  # web servers
        { from = 60000; to = 60050; } # mosh
        { from = 655;   to = 656; }   # tinc
      ];
      trustedInterfaces = [ "tun" ];  # tinc
    };

    networkmanager.enable = true;
  };

  ##################################################################################
  # Logging and system services
  ##################################################################################
  # dbus for xmonad
  services.dbus.packages = [ pkgs.gnome3.dconf ];

  # Enable UPower, which is used by taffybar
  services.upower.enable = true;
  systemd.services.upower.enable = true;
  systemd.services.acpid.enable = true;

  #JournalD settings
  services.journald.extraConfig = "Storage=persistent";

  environment.pathsToLink = [
    # Needed for GTK themes
    "/share"
  ];

  ##################################################################################
  # Fonts
  ##################################################################################
  fonts = {
    fontconfig.enable = true;
    enableFontDir = true;
    enableGhostscriptFonts = true;
    fonts = with pkgs; [
      fira
      fira-code
      fira-mono
      # font-droid
      inconsolata
      ubuntu_font_family
      nerdfonts
      libre-caslon
      libertinus
    ];
  };

  ##################################################################################
  # Authentication and security
  ##################################################################################
  # this is a huge pain, but it might be nessecary for protonmail-bridge
  # services.gnome3.gnome-keyring.enable = false;

  # use the gpg agent instead of the ssh agent
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
    wheelNeedsPassword = host.wheelNeedsPassword;
  };
}
