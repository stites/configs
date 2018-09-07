self: pkgs:

let
  lib = pkgs.stdenv.lib;

  plugins = with pkgs; [
    tmuxPlugins.resurrect
    tmuxPlugins.continuum
    tmuxPlugins.battery
    tmuxPlugins.cpu
    tmuxPlugins.sensible
  ];

  systemSettings = builtins.readFile ./global.conf;
  copypaste = builtins.readFile ./copypaste.conf;
  colorTheme = builtins.readFile ./theme.conf;
  # environment.systemPackages = plugins ++ [ tmuxinator ];

  addingPlugins = ''
    set -g @plugin 'tmux-plugins/tmux-sensible'
    set -g @plugin 'tmux-plugins/tmux-resurrect'
    set -g @plugin 'tmux-plugins/tmux-continuum'

    set -g @continuum-restore 'on'
    set -g @continuum-save-interval '60' # in minutes

    set -g @resurrect-strategy-nvim 'session'
    set -g @plugin 'tmux-plugins/tmux-battery'

    set -g status-right '#{cpu_icon} #{cpu_percentage} | #{battery_icon} #{battery_percentage} | Remaining: #{battery_remain} | %a %h-%d %H:%M '

    # Load Plugins
    ${lib.concatStrings (map (x: "run-shell ${x.rtp}\n") plugins)}
    '';

  paths = plugins ++ [ pkgs.tmuxinator ];
in
  {
    tmux = pkgs.tmux.override {
      # enable = true;
      program.tmux.extraTmuxConf = lib.concatStrings [ systemSettings copypaste colorTheme addingPlugins ];
    };
  }
  # }
  #symlinkJoin {
  #  name = "tmux-bundled";
  #  buildInputs = [ makeWrapper git ];
  #  paths = [ tmux ];

  #  # FIXME: this is the _bleeding edge_ -- use fetchFromGithub and manage scripts with nix!
  #  postBuild = ''
  #    # mkdir -p ~/.tmux/plugins/
  #    # for plugin in tpm tmux-resurrect tmux-continuum tmux-battery tmux-cpu tmux-sensible; do
  #    #   git clone https://github.com/tmux-plugins/$plugin       ~/.tmux/plugins/$plugin
  #    # done
  #    wrapProgram "$out/bin/tmux" --add-flags "-f ${./tmux.conf}"
  #  '';
  #};
#}
