{ ... }:
let
  secrets = import ./secrets.nix;
  pii = secrets.piis;
in
{
  home.file.".mailcap".text = ''
    text/html;  w3m -dump -o document_charset=%{charset} '%s'; nametemplate=%s.html; copiousoutput
  '';
  programs.afew.enable = true;
  programs.alot.enable = true;
  programs.notmuch.enable = true;
  programs.astroid = {
    enable = true;
    # externalEditor = "nvim-qt -- -c 'set ft=mail' '+set fileencoding=utf-8' '+set ff=unix' '+set enc=utf-8' '+set fo+=w' %1";
    extraConfig = {
    };
  };
  # services.protonmail-bridge.enable = true;
  programs.offlineimap.enable = true;
  # systemd.user.services.offlineimap = {
  #   Unit = {
  #     Description = "Offlineimap service";
  #     Requires = [ "protonmail-bridge.service" ];
  #     After    = [ "protonmail-bridge.service" ];
  #   };

  #   Service = {
  #     ExecStart ="${pkgs.offlineimap}/bin/offlineimap";
  #     Restart = "on-failure";
  #     RestartSec = "5s";
  #   };

  #   Install = {
  #     WantedBy = [ "default.target" ];
  #   };
  # };
  accounts.email = {
    maildirBasePath = ".maildir";
    accounts."${pii.address}" = {
      address = "${pii.address}";
      flavor = "plain";
      primary = true;
      realName = "Sam Stites";
      userName = "${pii.address}";
      # passwordCommand = "";
      signature = {
        showSignature = "append";
        text = ''
          cell: ${pii.cell}
          blog: ${pii.blog}
          more: ${pii.keybase}
        '';
      };
      astroid.enable = true;
      notmuch.enable = true;
      msmtp.enable = true;
      imap = {
        host = "127.0.0.1";
        port = 1143;
        # is broken, but connecting locally to bridge so should be ok
        tls.enable = false;
      };
      offlineimap = {
        enable = true;
        extraConfig ={
          account = {
            # full refresh, in min
            autorefresh = "0.2";

            # quick refreshs between each full refresh
            quick = 10;

            # update notmuch index after sync
            # postsynchook = "notmuch new";
          };
          local = {
            # delete remote mails that were deleted locally
            sync_deletes = "yes";
          };
          remote = {
            remotepass = secrets.offlineimap-key;
            keepalive = 60;
            holdconnectionopen = "yes";

            # delete local mails that were deleted on the remote server
            expunge = "yes";

            # sync only these folders
            folderfilter = "lambda fn: not (fn in ['All Mail', 'Folders/DMARC Reports'])";
          };
        };
      };
    };
  };
}
