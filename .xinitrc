xrdb -merge .Xresources &
setxkbmap -layout us -variant colemak -option 'ctrl:nocaps' &
set r rate 200 100 &
pgrep redshift &> /dev/null || redshift &> /dev/null &
xscreensaver -nosplash &
exec xmonad
