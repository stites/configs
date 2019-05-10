{ lib, pkgs, ... }:
let
  homefp = lib.strings.concatStringsSep "" [
    "00ffffffffffff0004726605a0ac37710d1b0103805825782aa195a9544d9c260f505"
    "4bfef80714f8140818081c081009500b300d1c04cc200a0f0403a600820c808706f31"
    "00001a509a00a0f0402e6008200808706f3100001a000000fd001e4b1e823c000a202"
    "020202020000000fc004163657220585233383243514b0118020334f14b0103041213"
    "05141f10075a23090707830100006a030c0020003878200000681a00000101304be66"
    "7d85dc4017880034cc200a0f0403a600820c808706f3100001a134c00a0f040176008"
    "209800706f3100001a2b5900a0a038274008209804706f3100001acd4600a0a0381f4"
    "030203a00706f3100001a00000034"
  ];
  edp-fp = lib.strings.concatStringsSep "" [
    "00ffffffffffff0006afeb2200000000001b0104a523137802d295a356529d270b5"
    "0540000000101010101010101010101010101010166d000a0f0703e803020350059"
    "c21000001866d000a0f07092843020350059c210000018000000000000000000000"
    "00000000000000000000002001036ff0f3c962a1c46962020200020"
  ];
  edp-defaults = {
    mode = "2880x1620";
    rate = "60.00";
  };
  hdmi-defaults = {
    enable = true;
    primary = true;
    position = "0x0";
    rate = "75.00";
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
        fingerprint = {
          eDP-1-1 = edp-fp;
        };
        config = {
          eDP-1-1 = edp-defaults // {
            enable = true;
            primary = true;
            position = "0x0";
          };
        };
      };
      "home" = {
        fingerprint = {
          eDP-1-1 = edp-fp;
          HDMI-0 = homefp;
        };
        config = {
          eDP-1-1 = edp-defaults // {
            enable = true;
            primary = false;
            position = "0x1600";
          };
          HDMI-0 = hdmi-defaults // {
            mode = "3840x1600";
          };
        };
      };
    };
  };
}
