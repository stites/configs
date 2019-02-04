{ config, pkgs, ... }:

let

  myMeshIp   = "10.0.6.124";
  myMeshMask = "255.255.255.255";
  myMeshName = "genbu";
  myBox      = "grothendieck";

in {

  security.sudo.extraRules = [ {
    users    = [ "tinc.${myMeshName}" ];
    commands = [ {
      command  = "${pkgs.nettools}/bin/ifconfig";
      options  = [ "NOPASSWD" ];
    } ];
  } ];

  environment.etc."tinc/${myMeshName}/tinc-up".source = pkgs.writeScript "tinc-up-${myMeshName}" ''
    #!${pkgs.stdenv.shell}
    ${pkgs.nettools}/bin/ifconfig $INTERFACE ${myMeshIp} netmask 255.255.255.0
  '';

  environment.etc."tinc/${myMeshName}/tinc-down".source = pkgs.writeScript "tinc-up-${myMeshName}" ''
    #!${pkgs.stdenv.shell}
    /run/wrappers/bin/sudo ${pkgs.nettools}/bin/ifconfig $INTERFACE down
  '';
  #/run/wrappers/bin/sudo ${pkgs.nettools}/bin/ifconfig $INTERFACE down

  # open tinc ports
  # ---------------
  # networking.firewall.allowedTCPPorts = [ 655 ];
  # networking.firewall.allowedUDPPorts = [ 655 ];

  # simple interface setup
  # ----------------------
  networking.interfaces."tinc.${myMeshName}" = {
    ipv4.addresses = [ { address = myMeshIp; prefixLength = 32; } ];
  };

  # configure tinc service
  # ----------------------
  services.tinc.networks."${myMeshName}"= {
    name          = myBox;      # who are we in this network.

    debugLevel    = 3;            # the debug level for journal -u tinc.private
    chroot        = false;        # otherwise addresses can't be a DNS
    interfaceType = "tun";        # tun might also work.

    extraConfig   = ''
      ConnectTo = genbu

      # Keys
      # ----
      # if you don't set the path as string, it will import the file in
      # in the nix/store where everybody can read it.
      PrivateKeyFile        = /home/stites/.ssh/tinc-1.1/${myBox}/rsa_key.priv
      Ed25519PrivateKeyFile = /home/stites/.ssh/tinc-1.1/${myBox}/ed25519_key.priv
    '';
    hosts = {
      # Address = grothendieck
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

      # Address = 209.6.46.152
      #erdos = ''
      #  Subnet = 10.0.6.89/32

      #  -----BEGIN RSA PUBLIC KEY-----
      #  MIICCgKCAgEAt8Uns8FuChMxRE8gi21nwDA1Gawprrue7oVz7EoQE8WUn24pbvX+
      #  u8S/Wh0xv45H/Br+UKrV32LqvDjhMXNq3/nvxzfoyirJ16CZDQyAP6Mtbav8ZMcZ
      #  X4n9PdjjuKplMkfZHzvIiabX31feJEq/znWcBByYDbOqfs5D9MkM3VTagbypzwX8
      #  byoALNcj/zpk+XHZjQhM+XJPcF2gqfiuiriy79pUGhl87Reva+EF0tum9KTjl4gJ
      #  5SPN0frmfGqzmKNrGztNpKE09egeaKOXHLOCRpibDpMHxkdw/W/bszi7KXTH5YWf
      #  4KW6zU/SY333jMnQHk26vF9FPTdBREKoUJ5SzjoBv5IjltV5BTTXCMzqs34jAEwh
      #  9I2znnklYmfPqpcH0lE4NfQd+Tjdfk+S7a8YwxdmoJGcPK0mUxQJfnla2uQYB3Uv
      #  YulsTLVJ44c6lIUAYxugUDgoMOrNiY6V0yTU2mH7VCoDu0eGvYX5pysWT0CXgMCq
      #  5xFhS57M03bHPFLZWBO4IfChWri6fIW/Z7aTR9OCb/xImbTaX3K3w1cpVZP7q3Or
      #  t+DyIZN3xcSHciBYFHqpZ6KTWuWU4FeT+mWK+AH0FI00eWI8EO3iaa3UpMJse3x+
      #  zpEu4U20Kktl4+qZnHY9bUG3gx7qUz0hw5IfWduOUBWxTAVZpR6LIiUCAwEAAQ==
      #  -----END RSA PUBLIC KEY-----
      #'';

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
      genbu = ''
        # Static Public IP: 138.197.96.179
        # Floating Public IP: 45.55.122.59
        # Subdomain name: genbu.stites.io

        Address = genbu.stites.io
        Subnet = 10.0.6.154/32
        Ed25519PublicKey = TBDhekt9YURKTKy80Jy2HAPh0F3p3SOO47AIopaFVSD

        #-----BEGIN RSA PUBLIC KEY-----
        #MIICCgKCAgEAxcI5g9+EjQh6g8wHKEKjt1D/eM14e6Gkd4eIFJ+9aTkV0ak5hPdm
        #G5TFmxL+UGUs5XO7R9L/XoEUCG6dbaqy0NAgHbIEMV7fY4o6t5xGfi9V2fUFEoYZ
        #HN+uKtdzeGI7cIdeBng0TOvwX2g3OxFJ+ju5nM2bTKhMgiWSmEPfEY+SqeFzjkIn
        #UHtdi3461wnoNEbUb+IAH3vM8DQR0Ic250DRGstUnSzg6VE8tIY5IWS5yBRyE9/n
        #OCKrKKcDqjhGeBER5bzVfciIc63+b4chGxSUEWvcFMRtHTeH1coxBK1837MS86XY
        #vU4mrYpox0kwO/yFw+3f8Iov5gGO93wA8dXyYfQZxLLWep3z6CzYfP4SgeLM55p6
        #hD36kR42WxDME6wmSSDPQ+ncbGFz4JGx+pRWGUS8waB0w8uvgI5qndnwz5WZn5Dk
        #+MqD58ru+7xGlvQdF7KfOcEDxvUphCB+y/umza9Xd/QTcGdD6bOIqbQ13M/h/6NK
        #9clOygtAuH1hGFWEJsgdntVk50UNu5aoFvAbGqCphCVh9AWeYd9r+BJcfnfwIKlh
        #KDe5GUAK3okaWNyTn05lZ8JDdVTFjeStiFO/xqhRMqRQfTcaRQlnJB7bCBiW2qzP
        #qVLxYjs//ww8GIB5IwJFTqVhg1kDfw9xmBpuyWuIaY00QlAbwSdoNrcCAwEAAQ==
        #-----END RSA PUBLIC KEY-----

        -----BEGIN RSA PUBLIC KEY-----
        MIICCgKCAgEAoYtjZZ2LBdc6Oe9LBVS3D5KM+J7M0yJmbwANBqoh1GOt1v4Uk/oq
        ngtiNpi+sCwKIpFO6tJoOv95vkymI8wqwnZuV7T0t5G7a5Vyh9f6I16G7/VjfEJw
        SwBofpto7WIUw1gK5QZJhBjyVjp4ejDPpPRfx7nJA7SBVMOTi3POk2qoCFVzUhPI
        YgP8eGg7mdR3b0DnP9ZCD0ZoE50bBgePGjjjx6WQ/GqoGkxTaahK1xKvURyeHpxu
        SZ/9BPbpxpBSuMLae8ByDiiqWAp5QnQPTkxGZ1LUJ6XdvMShrbzSyOSSg7NeE5Wc
        HUfiFqBhq1Lt/sDm46H8MUlyBt7SkbHMJyQNM7q5Jtt2GmHQiyzNLLG9cYF38q9s
        Edxpphlf5zSS5hz8gyBzksGm/0Dgt/fmUsHg0u4mzZbRt6kYAYclsyregqmkfaUP
        I1IXpUr2LOH2NJj8lRON8eIKHjNQUJyhNCAO4lHt7wFOxFg9n9CpCj91VpX9MTrr
        rH6dTB4RRCeAgYNkndyVbhPqKu552cXACPpjCu+P4I/+zZx3De7znrK81krrlQxK
        UI8lUI0fTnTgR7UCbzesaEo1L86L2uHekc2ZLEIhVsmIuGneDsklci5YCVm1J2Ug
        R73NnJAXEksqhqizPLxBL3jMWMSq2usbat2o21ixKaXUGUc4rzw6UmcCAwEAAQ==
        -----END RSA PUBLIC KEY-----
      '';

      hesse = ''
        Subnet = 10.0.6.213/32

        -----BEGIN OLD PUBLIC KEY-----
        MIICCgKCAgEA4XyVnsEQu4w6QfOFs3b73585ocDDGYUQ0xocEMHAp9tB03c/4ipT
        /LpjRZtS46FLxJ64F+cfDqQBWLrZvAhLoha/mc64p8YuD1QNRgMn9pL1Oorb+O14
        Ul6G573gspumkohfD4czYn0VTNmiQvZ5y7rsb0CdwyEi86hSaw7EkC5Qqjnl8Au/
        rr1ocYGiOXBeA5ZZpPJqPPmxMcE9BF18SSJl5iTgYCldn8tcUEsp1iuN1ZRWMkf8
        ADlThz8eWHzi0EGLUtMQ0Ma7ChyFR5iDdNRglTqOqVi1zWDH4TKxTKltMBKc7o+X
        l4O1nvky1SYNzLlOEeCnwYIfubxiVzD+bTHvr/VuZDYx/o/fw4ftd9NWNKzOe5Pm
        e0rCt6EDKw82FtQ7yKVhX5Pkmccojbop0Tfuzhds9LooxlMeV3raPDTuU6TNcNjm
        e+sKtaWqm4WB/1FInL2S8v9X3psFH/00/yGl7sEqL6IRzed6w22to0IUpqrep9Hw
        ZPjlZAwSpfFVD8mJ7Y1qZ1cpXO2Bz1KKuevRa+/Eui6Cvjii4CSRn2cds/xHtgJ4
        9wWxTdbQnaa1lxMq66xH4i05h/HnC/PduL+0xQsOU+nu8wbEbuA5JIi7CQtsluaJ
        CvdM9HDKAygcOhJ5RkGAccpK+ioyMF2H0Nhz1psUJpaB56Ye/Vl8x9sCAwEAAQ==
        -----END OLD PUBLIC KEY-----

        -----BEGIN RSA PUBLIC KEY-----
        MIICCgKCAgEAwB6bDkBWLy9uy+EVhzgPw7ez7CyPSbKiMdjMiRVca/lEH7m9cfmH
        YCZBr+BugsiHRsmXsO8Xo5+KvtvXHLBL28Rq9tKWBxGGBvFm9NFeyVZxdG9ac7wJ
        rczMXz+TPllOrBv+IDxEZa2YWeIK3HrGY46BFlCFxRM+k2B4PKv2xPtkIkeQXgFT
        h5tXG2cYMH/39uGXDLxry0V7a4E+p1REeidam215l9se2Z/s4591RcVToHYFXW4f
        ppeuXblU64rUKyO2Yg4/CsagDX3EKxuGy6iXbgZ5SR5sjLf2ev3ruaZCjsiY6mPZ
        XhK9XjXEcK6yqAV8UXXeY4MgclXfp8BHKwOVmvd6l09WFdrPTzv8U3yTVLFQdr3x
        cjQj4DWxXWY0ymF88UA8m21wvW3yRexSNww+Kt9AFQil8Cv5rtnP0Y0e2QiQVAuX
        vcRmL+Tk7mScP6PFtA8knWdJ+UWATcv2PsO2aSk1hr8CEmgdPLVJ8+eKaQqy5xU6
        Lct82J8JEH34iDzvgWweSwoA115CK5uPfahF3Np6fNVH809Ka7pY1y2KhFxMwcWP
        8WqMCcPi/MzB0B9foW++qKaF9kS65PrDcOap1Ezevpte3lqaCp597s6M6wxO7Yqu
        9XVrkSQxQaGvRWUf+Hl4AC/SkkbZLKDAxAmy9XNgRlDgDlGsxBcU8EcCAwEAAQ==
        -----END RSA PUBLIC KEY-----
      '';
      #kovalevskaya = ''
      #  Subnet = 10.0.6.103/32

      #  -----BEGIN RSA PUBLIC KEY-----
      #  MIICCgKCAgEA1Jw3R4mLcBLUcx1S63He8UsrRIgkwBdn/PLh/ALp+hZYhkeZaFCy
      #  vSGy1mboF8q3Uyuqu01OdU13ig1E9D5MbsgQxrtargDk8pXMTcmhokXtrCTk0409
      #  edIgtya/Tf+7nO4qL6KKYo9KCjue0JrudYWcu9B3471B25p2jiroVbk4ja2Uu1it
      #  f8WQP7eJJOiTrkYORAEFkVWGD8ubGgGDGWNM4NHERX4JhqjGTNgU4SRX5X8N/Nbe
      #  iwIEOkBFJOC6zB9twhqxNo1x1Be85ZUJl9fe9L14ZNfsPEGxnw3xhC3qzEPTZm/q
      #  LptP4S0S+NDqIb1c+G8unwNOIVs1TGUfZEfBDNnUrJOGlvqwdmcQQznvdBiEtv/Q
      #  ktCMIABhe3Due2B5HV4Jf8cWCCYxaFbyl1Wx5eYEt6Wv31DrLRawBTCxOey/DZyV
      #  uxO7H2xaqCtaKE5duOGKelML4fY0zt9fd509y25dSF0D6cLDtbP6tpIrZmF1wc49
      #  4Rl7C1ySNiFN3OwtUiZdDf86Czti2kb0ljMVO5jFeCpNE18xCD/i8BCOoFoOC1OQ
      #  NJfN04011j+JTzV3+D4da+PR64QlHrvmMS+BTsKUxXo5FTDtfoRqTlNnxcez9Rnh
      #  3hcSycS6+S2S/IqXUuqSvm13VjHbkDkQd4MvLsDsSfQ6GUAKuj0i1GUCAwEAAQ==
      #  -----END RSA PUBLIC KEY-----
      #'';
      mirzakhani = ''
        Subnet = 10.0.6.132/32
        #Ed25519PublicKey = J1U+2fpcnBYPvEud/XJTgpVcPFQLnPcPcTsjx76CkbJ

        #-----BEGIN RSA PUBLIC KEY-----
        #MIICCgKCAgEA2/gN8INXqNXvSmMQsIBKVXVFh0VbKFezh9mLv7LI54TuklhlaqRp
        #rrX7JqXG1lPxeaOho1F9y6l8/us896V0q9j/oJ//MyYJ03SEr2CAwQl7VMQ1xFhW
        #qhs9M662V+Me4Sae5JyEETf502lW+y7jM/ckTqbqyim6h0pZSeSZtWWYREIcbf5O
        #t8NAqOYQEyQVU/ph7LjkSn3akdCYOAr0Ypft9WRFRQK44Tq4eRuskv8iCPWBCsNT
        #g60U3b33c6Cxnv3JY6uALPraAsQk8mPuhj3E8+cs3Svc+tkkystPPaTyf4j814UG
        #G+xSttFLF2LujbCuboTvNH5wXcolc1w7xpZBp/d/o0aCh2k8geHZPmZ7i+e3yBHF
        #1mmwdq76gXuKJCiP838zyBh99MsCz2SFYCq71vKaMnMo5tJ2R5WPgM6dDONjeKhg
        #KxAysa+1Wi+nSD8JDki5ZoW3FSecqwI3CTL3I6Woc+SxHlWzK5pJNK6viOsfNW6c
        #jZVz1wqHLo+YjoQZZ5qD2ngIPVChY9fZaSST6j+a8Zlw2y1+M9b/H/EapezL7sJ4
        #KLT+RqYIdrShLih8TO6zcysOLKwgbIUyu5yz3/7bh4ej5E9F0EtXcq63cKCNvaaf
        #DNifkVx3tlEmmHJnr/m1TrtpiavsdmsX3JmlvcJJhR6RbRBo9sx00UUCAwEAAQ==
        #-----END RSA PUBLIC KEY-----
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
    };
  };
}
