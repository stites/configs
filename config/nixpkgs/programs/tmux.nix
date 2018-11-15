{ pkgs }:

with pkgs;

{
  enable = true;
  tmuxp.enable = true;

  plugins = [
    tmuxPlugins.battery
    {
      plugin = tmuxPlugins.resurrect;
      extraConfig = ''
        set -g @resurrect-strategy-nvim 'session'
        set -g @resurrect-dir '/home/stites/.tmux/resurrect'

      '';
    }
    {
      plugin = tmuxPlugins.continuum;
      extraConfig = ''
        set -g @continuum-restore 'on'
        set -g @continuum-save-interval '60' # minutes
      '';
    }
  ];
  extraConfig = ''
    # use vi mode
    set-window-option -g mode-keys vi

    # make sure new windows start from older ones
    bind c   new-window      -c "#{pane_current_path}"
    bind %   split-window -h -c "#{pane_current_path}"
    bind '"' split-window -v -c "#{pane_current_path}"

    # Sync panes
    bind y setw synchronize-panes

    # bind h select-pane -L
    # bind j select-pane -D
    # bind k select-pane -U
    # bind l select-pane -R
    # bind \ select-pane -l

    # resize panes
    bind-key -r J resize-pane -D 5
    bind-key -r K resize-pane -U 5
    bind-key -r H resize-pane -L 5
    bind-key -r L resize-pane -R 5

    # # Fixes for ssh-agent
    set -g update-environment "SSH_ASKPASS SSH_AUTH_SOCK SSH_AGENT_PID SSH_CONNECTION"
    # set-option -g default-shell /usr/local/bin/bash

    # # Stop renaming windows automatically
    set-option -g allow-rename off

    # But reorder windows automatically
    set-option -g renumber-windows on

    # ++++++++++++++++++++++++++++ #
    #        mouse control         #
    # ++++++++++++++++++++++++++++ #
    # clickable windows, panes, resizable panes:
    # set -g mouse-select-window on
    # set -g mouse-select-pane on
    # set -g mouse-resize-pane on

    # Enable mouse mode (tmux 2.1 and above)
    set -g mouse on

    # ++++++++++++++++++++++++++++ #
    #      use vi copy/paste       #
    # ++++++++++++++++++++++++++++ #
    # see: http://bit.ly/1LuQQ8h

    unbind [
    bind Escape copy-mode
    unbind p
    # https://unix.stackexchange.com/questions/67673/copy-paste-text-selections-between-tmux-and-the-clipboard#72340
    # https://unix.stackexchange.com/questions/131011/use-system-clipboard-in-vi-copy-mode-in-tmux
    bind p run "tmux set-buffer \"$(xclip -o)\"; tmux paste-buffer"
    bind-key -T copy-mode-vi 'v' send -X begin-selection
    bind-key -T copy-mode-vi 'V' send -X select-line
    bind-key -T copy-mode-vi 'r' send -X rectangle-toggle
    bind-key -T copy-mode-vi 'y' send -X copy-pipe-and-cancel "xclip -i -sel p -f | xclip -i -sel c"
    # bind-key -T copy-mode-vi 'y' send -X copy-pipe-and-cancel "xclip -in -selection clipboard"

    # move x clipboard into tmux paste buffer
    # move tmux copy buffer into x clipboard
    bind C-y run "tmux save-buffer - | xclip -i"

    # ++++++++++++++++++++++++++++ #
    #    set panes to start at 1   #
    # ++++++++++++++++++++++++++++ #
    set-window-option -g pane-base-index 1
    set-window-option -g base-index 1

    set -g status-bg blue
    set -g status-fg black

    # ++++++++++++++++++++++++++++ #
    #    change the prefix key     #
    # ++++++++++++++++++++++++++++ #
    set -g prefix C-b
    unbind C-b
    bind C-b send-prefix

    # +++++++++++++++++++++++++++++ #
    # set titles on and use un@host #
    # +++++++++++++++++++++++++++++ #
    set -g terminal-overrides "xterm*:XT:smcup@:rmcup@"
    set -g set-titles-string "#T"

    ######################
    ### DESIGN CHANGES ###
    ######################
    # taken from: http://www.hamvocke.com/blog/a-guide-to-customizing-your-tmux-conf/

    # panes
    set -g pane-border-fg black
    set -g pane-active-border-fg brightred

    ## Status bar design
    # status line
    set -g status-justify left
    set -g status-bg default
    set -g status-fg colour12

    # messaging
    set -g message-fg black
    set -g message-bg yellow
    set -g message-command-fg blue
    set -g message-command-bg black

    #window mode
    setw -g mode-bg colour6
    setw -g mode-fg colour0

    # window status
    setw -g window-status-format " #F#I:#W#F "
    setw -g window-status-current-format " #F#I:#W#F "
    setw -g window-status-format "#[fg=magenta]#[bg=black] #I #[bg=cyan]#[fg=colour8] #W "
    setw -g window-status-current-format "#[bg=brightmagenta]#[fg=colour8] #I #[fg=colour8]#[bg=colour14] #W "
    setw -g window-status-current-bg colour0
    setw -g window-status-current-fg colour11
    setw -g window-status-current-attr dim
    setw -g window-status-bg green
    setw -g window-status-fg black
    setw -g window-status-attr reverse

    # Info on left (I don't have a session display for now)
    set -g status-left ''''''

    # loud or quiet?
    set-option -g visual-activity off
    set-option -g visual-bell off
    set-option -g visual-silence off
    set-window-option -g monitor-activity off
    set-option -g bell-action none

    # set -g default-terminal "screen-256color"

    # The modes {
    setw -g clock-mode-colour colour135
    setw -g mode-attr bold
    setw -g mode-fg colour196
    setw -g mode-bg colour238

    # }
    # The panes {

    set -g pane-border-bg colour235
    set -g pane-border-fg colour238
    set -g pane-active-border-bg colour236
    set -g pane-active-border-fg colour51

    # }
    # The statusbar {

    set -g status-position bottom
    set -g status-bg colour234
    set -g status-fg colour137
    set -g status-attr dim
    set -g status-left ''''''
    set -g status-right '''#[fg=colour233,bg=colour241,bold] %d/%m #[fg=colour233,bg=colour245,bold] %H:%M:%S '
    set -g status-right-length 50
    set -g status-left-length 20

    setw -g window-status-current-fg colour81
    setw -g window-status-current-bg colour238
    setw -g window-status-current-attr bold
    setw -g window-status-current-format ' #I#[fg=colour250]:#[fg=colour255]#W#[fg=colour50]#F '

    setw -g window-status-fg colour138
    setw -g window-status-bg colour235
    setw -g window-status-attr none
    setw -g window-status-format ' #I#[fg=colour237]:#[fg=colour250]#W#[fg=colour244]#F '

    setw -g window-status-bell-attr bold
    setw -g window-status-bell-fg colour255
    setw -g window-status-bell-bg colour1

    # }
    # The messages {

    set -g message-attr bold
    set -g message-fg colour232
    set -g message-bg colour166

    # }
    #######################################################
    #######################################################
    #######################################################
    #######################################################
    ###set -g @plugin 'tmux-plugins/tpm'

    ###set -g @plugin 'tmux-plugins/tmux-sensible'
    ###set -g @plugin 'tmux-plugins/tmux-resurrect'
    ###set -g @plugin 'tmux-plugins/tmux-continuum'

    ###set -g @continuum-restore 'on'
    ###set -g @continuum-save-interval '60' # in minutes

    ###set -g @resurrect-strategy-nvim 'session'
    ###set -g @plugin 'tmux-plugins/tmux-battery'
    set -g status-right '#{battery_icon} #{battery_percentage} | Remaining: #{battery_remain} | %a %h-%d %H:%M '

    # Smart pane switching with awareness of vim splits
    is_vim='echo "#{pane_current_command}" | grep -iqE "(^|\/)g?(view|n?vim?)(diff)?$"'
    bind -n C-j if-shell "$is_vim" "send-keys C-j" "select-pane -D"
    bind -n C-k if-shell "$is_vim" "send-keys C-k" "select-pane -U"

    bind -n C-h if-shell "$is_vim" "send-keys C-h" "select-pane -L"
    bind -n C-l if-shell "$is_vim" "send-keys C-l" "select-pane -l"

    # bind -n C-\ if-shell "$is_vim" "send-keys C-\\" "select-pane -l"

    set -ag terminal-overrides ',screen*:cvvis=\E[34l\E[?25h'

    # Restoring Clear Screen (C-l) <<< This is blocking the above
    bind C-l send-keys 'C-l'
    bind C-k send-keys 'C-k'
    bind C-u send-keys 'C-u'

    # Start GoTTY in a new window with C-t
    bind-key C-t new-window "gotty tmux attach -t `tmux display -p '#S'`"

    ###### Enable tpm (tmux package manager)
    #####run '~/.tmux/plugins/tpm/tpm'
  '';

}
