xrdb -merge .Xresources &
setxkbmap -layout us -variant colemak -option 'ctrl:nocaps' &
set r rate 200 100 &
redshift &
exec ~/.local/bin/xmonad
