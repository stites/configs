{ lib, pkgs, ... }:
let
  usbc-dock-monitor = {
    DP1-1 = lib.strings.concatStringsSep "" [
      "00ffffffffffff0004726605a0ac37710d1b0103805825782aa195a9544d9c260f5054bfe"
      "f80714f8140818081c081009500b300d1c04cc200a0f0403a600820c808706f3100001a50"
      "9a00a0f0402e6008200808706f3100001a000000fd001e4b1e823c000a202020202020000"
      "000fc004163657220585233383243514b0118020334f14b010304121305141f10075a2309"
      "0707830100006a030c0010003878200000681a00000101304be667d85dc4017880034cc20"
      "0a0f0403a600820c808706f3100001a134c00a0f040176008209800706f3100001a2b5900"
      "a0a038274008209804706f3100001acd4600a0a0381f4030203a00706f3100001a0000004"
      "4"
    ];
  };
  laptop-monitor = {
    eDP1 = lib.strings.concatStringsSep "" [
      "00ffffffffffff0006af362300000000001b0104a51f117802f4f5a4544d9c270f5054000"
      "00001010101010101010101010101010101e65f00a0a0a040503020350035ae1000001800"
      "00000f0000000000000000000000000020000000fe0041554f0a202020202020202020000"
      "000fe004231343051414e30322e33200a00b2"
    ];
  };
  edp-defaults = {
    mode = "2560x1440";
    rate = "60.00";
  };
  external-defaults = {
    enable = true;
    primary = true;
    position = "0x0";
    rate = "30.00";
  };
in
{
  programs.autorandr = {
    enable = true;
    # hooks.predetect = {
    #   sleepabit = ''
    #     sleep 2
    #   '';
    # };
    profiles = {
      "laptop" = {
        fingerprint = laptop-monitor;
        config = {
          eDP1 = edp-defaults // {
            enable = true;
            primary = true;
            position = "0x0";
          };
        };
      };
      "lab" = {
        fingerprint = usbc-dock-monitor // laptop-monitor;
        config = {
          eDP1 = edp-defaults // {
            enable = true;
            primary = false;
            position = "0x1600";
          };
          DP1-1 = external-defaults // {
            mode = "3840x1600";
          };
        };
      };
    };
  };
}
