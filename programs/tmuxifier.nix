{ pkgs, lib, ... }:
let
  homedir = builtins.getEnv "HOME";
in
{
  home.file.".tmuxifier" = {
      recursive = true;
      source = pkgs.fetchFromGitHub {
        owner = "jimeh";
        repo = "tmuxifier";
        rev = "v0.13.0";
        sha256 = "1b6a1cw2mnml84k5vhbcp58kvp94xlnlpp4kwdhqw4jrzfgcjfzd";
      };
      onChange =
        let
          mkide = ishs: lib.strings.concatStringsSep "\n" [
            # Set window root path. Default is `$session_root`.
            # Must be called before `new_window`.
            # ESCAPED BECAUSE WE ARE PIPING TO STDIN TO GENERATE THESE FILES
            ''window_root "\$(pwd)"''

            # Create new window. If no argument is given, window name will be based on
            # layout file name.
            # ESCAPED BECAUSE WE ARE PIPING TO STDIN TO GENERATE THESE FILES
            ''new_window "\$(pwd | sed 's@.*/@@')"''

            # |--------- ---------|
            # |         |         |
            # |         |    2    |
            # |    1    |---------|
            # |         |         |
            # |         |    3    |
            # |         |---------|
            # |         |    4    |
            # |         |---------|
            # |         |    5    |
            # |         |---------|
            # |         |    6    |
            # |--------- ---------|

            # Split window into panes.
            "split_h"
            # select_pane 1
            # split_v 8
            "select_pane 2"
            "split_v"
            "split_v"
            "split_v"
            (if ishs then "split_v" else "")

            # Run commands.
            (if ishs
              then ''run_cmd "vim Setup.hs +Ghcid" 1''
              else ''run_cmd "vim" 1'')

            # run_cmd "make ghcid && echo \"ghcid ran from make\"
                   #  || ghcid --height=8 --command=\"stack ghci --test\"" 2
            ''run_cmd "git status"  2''
            ''run_cmd "${if ishs then "stack ghci --test" else "ipython"}"     3''
            (if ishs
              then ''run_cmd "sos -p '([^_].*\.py)'               -c 'python \1'" 4''
              else ''run_cmd "sos -p 'app/.*\.hs' -p 'src/.*\.hs' -c 'hlint  \0'" 4'')
            (if ishs
              then ''run_cmd "sos -p '([^_].*\.py)' -c 'ctags -R --extra=+f --python-kinds=-i'" 5''
              else ''run_cmd "sos -p '.*\.hs'       -c 'codex update --force'"                  5'')
            ''run_cmd "vim .stites" 6''

            # Set active pane.
            "select_pane 1"
          ];
          hside = mkide true;
          pyide = mkide false;
        in
        ''
          cat >${homedir}/.tmuxifier/layouts/py.window.sh <<EOL
          ${pyide}
          EOL
          cat >${homedir}/.tmuxifier/layouts/hs.window.sh <<EOL
          ${hside}
          EOL
        '';
    };
}
