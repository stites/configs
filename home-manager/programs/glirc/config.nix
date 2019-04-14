{ lib, ... }:

let
  secrets = import ../../secrets.nix;
  homedir = builtins.getEnv "HOME";
in
{
  config = lib.strings.concatStringsSep "\n" [
    # Grab the Vim syntax highlighting file from the config-value package
    # Learn more about these settings with `glirc2 --config-format`
    "-- vim: filetype=config-value"
    ''
    activity-bar: yes
    layout: two-column
    extra-highlights: ["glirc", "lens"]

    nick-padding:
       side: left -- try right if you don't like left padding
       width: 13
    ''

    # "open" works on macOS, "gnome-open" for GNOME
    "url-opener: \"firefox\""

    # Apparently this is broken
    # "download-dir: \"${homedir}/Downloads\""

    # Defaults used when not specified on command line
    ''
    defaults:
      nick:            "stites"
      username:        "stites"
      realname:        "stites"
      tls:             yes-insecure
      tls-client-cert: "${homedir}/.ssh/znc/certificate.pem"
      tls-client-key:  "${homedir}/.ssh/znc/key.pem"
      log-dir:         "${homedir}/.local/share/data/glirc/irclogs"
    ''

    # Override the defaults when connecting to specific servers
    ''
    servers:
      * name: "znc/freenode"
        hostname:      "mirzakhani"
        port: 7777
        username: "stites/freenode"
        password: "${secrets.irc.znc.password}"
        autoconnect: yes

      * name: "znc/gitter"
        hostname:      "mirzakhani"
        port: 7777
        username: "stites/gitter"
        password: "${secrets.irc.znc.password}"
        autoconnect: yes
        nick-completion: slack
        -- connect-cmds:
        --   * "join #favoritechannel,#otherchannel"
        --   * "msg mybot another command"
    ''

    ''
    macros:
      * name: "wipe"
        commands:
          * "clear"
          * "znc *status clearbuffer $channel"

      * name: "mysplits"
        commands:
          * "splits znc/gitter:#dataHaskell/Lobby znc/freenode:#haskell znc/freenode:#vim znc/freenode:#nixos znc/freenode:#mlpack"
    ''
    # -- Example use of macro in combination with an extension
    # * name: "extra"
    #   commands:
    #     * "extension Lua some-parameter $network $channel"

    ''
    key-bindings:
      * bind: "C-M-b"
        command: "masks b"
      * bind: "C-M-q"
        command: "masks q"
      * bind: "C-M-k"
        command: "clear"
    ''

    ''
    palette:
      line-marker: yellow
      time:
        fg: [10,10,10] -- RGB values for color for timestamps
        bg: blue
      nick-colors:
        [ cyan, magenta, green, yellow, blue
        , bright-cyan, bright-magenta, bright-green, bright-blue
        , 218,  88,  89, 124, 160, 205, 212, 224 -- reds
        ,  94, 130, 166, 172, 208, 214, 216, 180 -- oranges
        ,  58, 226, 229, 184, 187, 100, 142, 220 -- yellows
        ,  22,  34,  40,  82,  70,  64,  48,  85 -- greens
        ,  25,  27,  33,  39,  51,  80,  81,  75 -- blues
        ,  69,  61,  56,  54, 129,  93,  99, 147 -- purples
        ]
    ''
  ];
}
