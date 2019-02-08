{ pkgs, lib, config, ... }:

let
  modifier = "Mod4";
  move = "50px";
in
{
	home.keyboard.layout = "en";
	xsession.enable = true;
        set $mode_system System (l) lock, (e) logout, (s) suspend, (h) hibernate, (r) reboot, (Shift+s) shutdown, (Shift+p) Presenter Modus
        mode "$mode_system" {
          bindsym l exec --no-startup-id ~/.config/i3/i3-exit lock, mode "default"
          bindsym e exec --no-startup-id ~/.config/i3/i3-exit logout, mode "default"
          bindsym s exec --no-startup-id ~/.config/i3/i3-exit suspend, mode "default"
          bindsym h exec --no-startup-id ~/.config/i3/i3-exit hibernate, mode "default"
          bindsym r exec --no-startup-id ~/.config/i3/i3-exit reboot, mode "default"
          bindsym Shift+s exec --no-startup-id ~/.config/i3/i3-exit shutdown, mode "default"
          bindsym Shift+p exec --no-startup-id ~/.config/i3/i3-exit present, mode "default"
          # back to normal: Enter or Escape
          bindsym Return mode "default"
          bindsym Escape mode "default"
        }
      '';
      config = {
        modifier = "${modifier}";
        focus.forceWrapping = true;
        keybindings =
          lib.mkOptionDefault {
            "${modifier}+1" = "workspace 1:Web";
            "${modifier}+2" = "workspace 2:Email";
            "${modifier}+3" = "workspace 3:Terminal";
            "${modifier}+Shift+1" = "move container to workspace 1:Web";
            "${modifier}+Shift+2" = "move container to workspace 2:Email";
            "${modifier}+Shift+3" = "move container to workspace 3:Terminal";
            "${modifier}+Shift+Left" = "move left ${move}";
            "${modifier}+Shift+Down" = "move down ${move}";
            "${modifier}+Shift+Up" = "move up ${move}";
            "${modifier}+Shift+Right" = "move right ${move}";
            "${modifier}+space" = "focus mode_toggle";
            "${modifier}+a" = "focus parent";
            "${modifier}+SHIFT+plus" = "move scratchpad";
            "${modifier}+plus" = "scratchpad show";
            "${modifier}+x" = "move workspace to output right";
            "${modifier}+y" = "move workspace to output up";
            "Control+${modifier}+a" = "workspace prev";
            "Control+${modifier}+s" = "workspace next";
            "Control+${modifier}+q" = "workspace back_and_forth";
            "${modifier}+Tab" = "exec rofi -show combi run -threads 0";
            "${modifier}+Shift+e" = "mode \"\$mode_system\"";
            "XF86AudioRaiseVolume" = "exec ${pkgs.alsaUtils}/bin/amixer -q set Master 5%+ unmute";
            "XF86AudioLowerVolume" = "exec ${pkgs.alsaUtils}/bin/amixer -q set Master 5%- unmute";
            "XF86AudioMute" = "exec ${pkgs.alsaUtils}/bin/amixer -q set Master 1+ toggle";
            "XF86MonBrightnessUp" = "exec xbacklight -inc 10";
            "XF86MonBrightnessDown" = "exec xbacklight -dec 10";
          };
        modes = {
          resize = {
            j = "resize shrink left width 10 px or 10 ppt";
            "Shift+J" = "resize grow left width 10 px or 10 ppt";
            k = "resize shrink down 10 px or 10 ppt";
            "Shift+K" = "resize grow down 10 px or 10 ppt";
            l = "resize shrink up 10 px or 10 ppt";
            "Shift+L" = "resize grow up 10 px or 10 ppt";
            odiaeresis = "resize shrink right 10 px or 10 ppt";
            "Shift+Odiaeresis" = "resize grow right 10 px or 10 ppt";
            Left = "resize shrink left 10 px or 10 ppt";
            "Shift+Left" = "resize grow left 10 px or 10 ppt";
            Down = "resize shrink down 10 px or 10 ppt";
            "Shift+Down" = "resize grow down 10 px or 10 ppt";
            Up = "resize shrink up 10 px or 10 ppt";
            "Shift+Up" = "resize grow up 10 px or 10 ppt";
            Right = "resize shrink right 10 px or 10 ppt";
            "Shift+Right" = "resize grow right 10 px or 10 ppt";
            Return = "mode \"default\"";
            Escape = "mode \"default\"";
          };
        };
        startup = [
          {
            command = "i3-msg 'workspace 1; append_layout /home/noah/.i3/workspace-1.json'";
            notification = false;
          }
        ];
        bars = [
          {
            id = "bar-0";
            position = "top";
          }
        ];
      };
    };
  };
 }