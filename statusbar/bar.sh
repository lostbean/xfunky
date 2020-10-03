#!/bin/sh

cd "$( dirname "${BASH_SOURCE[0]}" )"

echo '' >/dev/shm/xfunky_conky 
echo '' >/dev/shm/xfunky_dbus 

function printBar() {
    echo "$(</dev/shm/xfunky_conky) ^p(+100) $(</dev/shm/xfunky_dbus)"
}

function statusGen() {
    (conky -c conky.lua | (while true; do read foo; echo $foo >/dev/shm/xfunky_conky; printBar; done;))&
    (lua dbus.lua | (while true; do read foo; echo $foo >/dev/shm/xfunky_dbus; printBar; done;))&
}


xpos=0
ypos=0
width=1400
height=20
fgcolor="#f0f0f0"
bgcolor="#0f0f0f"
font="-*-fixed-medium-*-*-*-12-*-*-*-*-*-*-*"

parameters=" -dock -x $xpos -y $ypos -h $height" 
parameters+=" -fn $font"
parameters+=" -ta l -bg $bgcolor -fg $fgcolor"
parameters+=" -title-name xfunky"

pkill dzen2

statusGen | dzen2 $parameters &
