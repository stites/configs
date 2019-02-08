let
  root = "/var/lib/weechat";
in
{
  systemd.user.services.weechat = {
    environment.WEECHAT_HOME = "${root}";
    serviceConfig = {
      User = "stites";
      Group = "users";
      # RemainAfterExit = "yes";
    };
    script = "${pkgs.weechat}/bin/weechat-headless --colors";
    wantedBy = [ "multi-user.target" ];
    wants = [ "network.target" ];
  };
}
