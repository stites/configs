{ config, pkgs, ... }:

let
  host = import ../host.nix;
in {

  security.sudo.extraRules = [ {
    users    = [ "tinc.${host.tinc.netname}" ];
    commands = [ {
      command  = "${pkgs.nettools}/bin/ifconfig";
      options  = [ "NOPASSWD" ];
    } ];
  } ];

  environment.etc."tinc/${host.tinc.netname}/tinc-up".source = pkgs.writeScript "tinc-up-${host.tinc.netname}" ''
    #!${pkgs.stdenv.shell}
    ${pkgs.nettools}/bin/ifconfig $INTERFACE ${host.tinc.ip} netmask 255.255.255.0
  '';

  environment.etc."tinc/${host.tinc.netname}/tinc-down".source = pkgs.writeScript "tinc-up-${host.tinc.netname}" ''
    #!${pkgs.stdenv.shell}
    /run/wrappers/bin/sudo ${pkgs.nettools}/bin/ifconfig $INTERFACE down
  '';

  # /run/wrappers/bin/sudo ${pkgs.nettools}/bin/ifconfig $INTERFACE down

  # open tinc ports
  # ---------------
  # networking.firewall.allowedTCPPorts = [ 655 ];
  # networking.firewall.allowedUDPPorts = [ 655 ];

  # simple interface setup
  # ----------------------
  networking.interfaces."tinc.${host.tinc.netname}" = {
    ipv4.addresses = [ { address = host.tinc.ip; prefixLength = 32; } ];
  };

  # configure tinc service
  # ----------------------
  services.tinc.networks."${host.tinc.netname}"= {
    name          = host.name;    # who are we in this network.
    debugLevel    = 3;            # the debug level for journal -u tinc.private
    chroot        = false;        # otherwise addresses can't be a DNS
    interfaceType = "tun";        # tun might also work.

    extraConfig   = ''
      # TODO: move this from "netname" to "gateways"
      ConnectTo = ${host.tinc.netname}
      PrivateKeyFile        = ${host.tinc.privateKeys.rsaFile}
      Ed25519PrivateKeyFile = ${host.tinc.privateKeys.ed25519File}
    '';

    hosts = {
      grothendieck = ''
        Subnet = 10.0.6.124/32
        Ed25519PublicKey = XWVgslSOX9rRXTdv/3PjDR/9UbSdsH0dbe2+pQ3f6sA

        -----BEGIN RSA PUBLIC KEY-----
        MIICCgKCAgEAuu6l9rctnasRZ33balR3JKUsND8izAHzjlmzmPPteztO7nAc0Rnj
        TUYG86p+GIzYwWjOE471QmzFxKBWupmBznxX/ubwuEh0KvD+oO/L80ceFfHWsNCr
        DEsakN5dpfbMhId0KwC4vLm/SyIJ4baVHzwXAfvsbFAygEWHjHTFOcgxO2X3xyz1
        zfkykvx7qAQeJ4ZvmE6ZVk40F1qADjlVIHJdWDrk+4ZGobPV4/3Jl8H9/+IguzD6
        67xz0wY1xmeybAjxbfi3a+bY80MwA8N6iiiNE3gb2477rRK7D2xy/JrAhm+4XRqX
        qEj6babAJY9eAKxMN61DNWFRvKHXlyKQC4fDr+bpfNp8qgu5Uk3j+U8BVRc/oMIy
        qBWVgjzDhcKvDguruv2PMbAUhrArwF05J0YkkO+QSp3XJeEIhD0OPkn0K2utC7Z0
        EGIqYhU/1dQNM7TkbFTErmaUxneoRl/MjkuCXAaRAFzAlqmMECgCbDMwQRO5vgvG
        6mkK3w8HWbTtr2axMuds4LWDFEzxjThdmb7WdDPkuTWcC3JMsH071R+vWVbDLV5S
        3g9TwuuSvTK4hDtgjkUR21Avr+JWbFdfd7FBm9iph3hmacoL83lYmO5AHsdR0fAM
        ppinOywsIe9H1ZMJrXxIPoC3Zz9BvWZA5gVAXRBrYC6pE2F5jxdE/p8CAwEAAQ==
        -----END RSA PUBLIC KEY-----
      '';

      erdos = ''
        Subnet = 10.0.6.89/32
        Ed25519PublicKey = FYQtkThIU/XqzsSbK8vNDwFShrgS3diXfF8/pInSdaC
      '';

      #gauss = ''
      #  Subnet = 10.0.6.48/32

      #  -----BEGIN RSA PUBLIC KEY-----
      #  MIICCgKCAgEA761S2aXK2cURVWbAew7yEL7U1RhV4fOVucfNvx/jLzEiIO+BikCY
      #  d2e3WHzrmbQi1fo/p794hCOq2JByhytRH1G/OGgqMQPq6dxJEPqhF/1aTyVd1YDX
      #  0206uVabyNpl6WAPt2aGAgCtl+5ALFFF0DhLnEmwFMOAB7DwJ0QrS2RUYG2txgZw
      #  stgIe519zOcF45hF1LppXZqZmptH3HcB7d3hI+1R+S0bf/Z7msxASntN1MHKmjaH
      #  wusFU2S55pYDONbh5EC9+ggOCk2bE/B8J1nfADs/bYAgVaU+6b5LikFL0Q88uadZ
      #  Qgvg67MXwWQT920r1mPUP0XEzKlzbTlUduI86uA0XxUZLPYxqgnf0472OFUN5dVq
      #  7nJ1ZNkRy7oIHi4xvsCLZEGPwKE7h5MbD1EpJxzBrcxgl4MuBiQXD1G8VEJdQeU6
      #  XA+ybEhmVjcoWny6FgTuCzUW6kvciW+ntLF96cLKm/qT356hpGsy4hwHHEOL5Jyt
      #  SEZsOG9YyqJCULCpdd2jFr0fcB1aR2eDH+VjrgcvOwEPC5tReFOJTPkLTcsgVpWJ
      #  DOQ/HRGpcUgEApIWgleQJkgX9VZZ/s0OCTpaqUTE2eO8IUtALXxUpWxyflyESp9V
      #  dQDJA0LWoSaFFEejxnsxk77uLJ6rRGvDtFB4a/gDyOBiI5r0ls/PYhcCAwEAAQ==
      #  -----END RSA PUBLIC KEY-----
      #'';

      #hesse = ''
      #  Subnet = 10.0.6.213/32

      #  -----BEGIN RSA PUBLIC KEY-----
      #  MIICCgKCAgEAwB6bDkBWLy9uy+EVhzgPw7ez7CyPSbKiMdjMiRVca/lEH7m9cfmH
      #  YCZBr+BugsiHRsmXsO8Xo5+KvtvXHLBL28Rq9tKWBxGGBvFm9NFeyVZxdG9ac7wJ
      #  rczMXz+TPllOrBv+IDxEZa2YWeIK3HrGY46BFlCFxRM+k2B4PKv2xPtkIkeQXgFT
      #  h5tXG2cYMH/39uGXDLxry0V7a4E+p1REeidam215l9se2Z/s4591RcVToHYFXW4f
      #  ppeuXblU64rUKyO2Yg4/CsagDX3EKxuGy6iXbgZ5SR5sjLf2ev3ruaZCjsiY6mPZ
      #  XhK9XjXEcK6yqAV8UXXeY4MgclXfp8BHKwOVmvd6l09WFdrPTzv8U3yTVLFQdr3x
      #  cjQj4DWxXWY0ymF88UA8m21wvW3yRexSNww+Kt9AFQil8Cv5rtnP0Y0e2QiQVAuX
      #  vcRmL+Tk7mScP6PFtA8knWdJ+UWATcv2PsO2aSk1hr8CEmgdPLVJ8+eKaQqy5xU6
      #  Lct82J8JEH34iDzvgWweSwoA115CK5uPfahF3Np6fNVH809Ka7pY1y2KhFxMwcWP
      #  8WqMCcPi/MzB0B9foW++qKaF9kS65PrDcOap1Ezevpte3lqaCp597s6M6wxO7Yqu
      #  9XVrkSQxQaGvRWUf+Hl4AC/SkkbZLKDAxAmy9XNgRlDgDlGsxBcU8EcCAwEAAQ==
      #  -----END RSA PUBLIC KEY-----
      #'';
      mirzakhani = ''
        Subnet = 10.0.6.132/32

        -----BEGIN RSA PUBLIC KEY-----
        MIICCgKCAgEAuS1hzfe1hHP7sn7/o3t/+iIA/yLuhge0mjqHomMtJI0FqJaOxXFa
        wSfTtbnVNrNmG6T+AO7UZc8ST4qfi4TF41sWV/84OykoRBY08/tvwAAKvTr5uqyP
        nesfz2tOhn20BUzC2ZTr3FLxLhBq5YNX/dKD5iVVbJ7QZHdq+4U10GfujB8V1tPs
        n4OvI4TEo81JufamyqopBeZ6VSBfhaSg92YtbFhKkoKuAKnb8ruZp8zwMf2yfDt/
        JBeXSNMeHDMj/EjdVj8wVNDUlWznZj7n4dpJRHbpm2C1oWwmnec7B42dMUHJ2Kwd
        lqayyRILBG2M5Dz36+sYs+o3CMEZLcKgL9aJ2IQVynY7X/zO18TfmAJtjcpKQ0ck
        8W52/fZPbewazG0QKNE68Yg88W9mW6kImP2kKmEb6YECRGs3buC4YUOzKtW3lhf1
        BA8nsmIDuiZJvjEdkzmEcRxWD9U2s8+0lqTSITZ2D1I9DiazKQTjoBSnroMCnWyS
        dWirzrvgxx82R5boci7igAYIEGXjCRZIyGFR8XvZgKY27bwQLp7s0d0JTfbvp87t
        M2eDWM5qxv7mlC/OCNlMROaeToS33ExyX8+mqiupydVVzEKla8MKqQco7CRofirl
        Rpp/Qq9bXRct1EZ2aIqsHm5rIwOYNcDuZ8gblwL8l2kOvDv1vC2LZ+0CAwEAAQ==
        -----END RSA PUBLIC KEY-----
        Ed25519PublicKey = 3psE68x13ChreSu2iHywklM36Q6Ij2rhdS9bTJRJkMI
      '';
    } // host.tinc.gateways;
  };
}
