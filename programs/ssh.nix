let
  secrets = import ../secrets.nix;
in
{
  programs.keychain = {
    enable = true;
    enableBashIntegration = true;
    enableZshIntegration = false;
    agents = [ "ssh" "gpg" ];
    inheritType = null;
    keys = [ "id_rsa" "id_ed25519" secrets.gpg.signing-key ];
  };
  # Just use nixos-gnome-keyring for this
  services.gnome-keyring = {
    enable = true;
    components = [
      # Public-Key Cryptography Standards # 11 for token interface and token behavior
      "pkcs11"   # Manage certificates

      # A store where GNOME applications can store and find passwords and other sensitive data.
      "secrets"  # Required for protonmail-bridge

      # Use the gnome kering ssh agent which uses X.509 and/or OpenSSH encryption keys.
      "ssh" # << use keychain (or gpg-agent) for this
    ];
  };
  programs.ssh = {
    enable = true;
    forwardAgent = true;
    serverAliveInterval = 120;
    compression = true;
    controlMaster = "auto";
    controlPath = "/tmp/%r@%h:%p";
    controlPersist = "10m";
    matchBlocks = {
      # Monad network
      State  = { hostname = "192.168.1.1"; };
      Cont   = { hostname = "192.168.1.7"; user = "root"; };
      PiHole = { hostname = "192.168.1.19"; user = "adblock"; };

      # Turtle network
      gauss = { hostname = "10.0.6.48"; };
      erdos = { hostname = "10.0.6.89"; };
      genbu = { hostname = "10.0.6.154"; };
      lambek = { hostname = "10.0.6.63"; };
      grothendieck = { hostname = "10.0.6.124"; };
      kovalevskaya = { hostname = "10.0.6.103"; };
      mirzakhani = {
        hostname = "10.0.6.132";
        forwardX11Trusted = true;
      };
      # ssh root@10.11.99.1 "mkdir -p ~/.ssh && \
      #    touch .ssh/authorized_keys && \
      #    chmod -R u=rwX,g=,o= ~/.ssh && \
      #    cat >> .ssh/authorized_keys" < ~/.ssh/id_rsa.pub
      remarkable = {
        hostname = "192.168.1.112";
        # hostname = "10.11.99.1";
        user = "root";
        # This _must_ be rsa because of reMarkable's sshd client
        identityFile = "${builtins.getEnv "HOME"}/.ssh/id_rsa";
      };
    };
  };
}
