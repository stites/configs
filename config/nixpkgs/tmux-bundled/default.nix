{ git, tmux, writeText, symlinkJoin, makeWrapper }:

symlinkJoin {
  name = "tmux-bundled";
  buildInputs = [ makeWrapper git ];
  paths = [ tmux ];

  # FIXME: this is the _bleeding edge_ -- use fetchFromGithub and manage scripts with nix!
  postBuild = ''
    # mkdir -p ~/.tmux/plugins/
    # for plugin in tpm tmux-resurrect tmux-continuum tmux-battery tmux-cpu tmux-sensible; do
    #   git clone https://github.com/tmux-plugins/$plugin       ~/.tmux/plugins/$plugin
    # done

    wrapProgram "$out/bin/tmux" --add-flags "-f ${./tmux.conf}"
  '';
}
