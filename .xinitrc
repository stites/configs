# important for setting styles
export GTK2_RC_FILES="$HOME/.gtkrc-2.0"

# load X resources (fixes font issues)
test -r $HOME/.Xdefaults && xrdb -I$HOME -merge $HOME/.Xdefaults &

# Start screen saver
xscreensaver -no-spash &
# xautolock -time 15 -locker "xlock -mode qix" &

# Set background
# nitrogen --restore &

# Activate primary & secondary screen
#xrandr --output eDP-1 --primary --auto
#xrandr --output HDMI-1 --auto --left-of eDP-1


# manage user directories
xdg-user-dirs-update &

# add flux
redshift &

# use basic graphics
#compton &

# start up xmonad
exec xmonad
