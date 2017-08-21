xrdb -merge .Xresources &
setxkbmap -layout us -variant colemak -option 'ctrl:nocaps' &
pgrep redshift &> /dev/null || redshift &> /dev/null &
xscreensaver -nosplash &
set r rate 200 100 &
exec xmonad
