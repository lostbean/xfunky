Dual Head Config
================

Use either twinview (NVidia) or Xrandr. In last case, use Xinerama.

Build Tips
==========

Use the command "debuild -us -uc" to build and pack the debian package [http://wiki.debian.org/IntroDebianPackaging]

Compiling without xinerama will make xmonad see the dual head screen as one big screen (extended mode).


Configutarion for Gnome-3.x
===========================

Using gnome-tweak-tool, set off the desktop managment by the file manager in order to focus the Workspace whenever the mouse moves to an empty one.

Copy usr/share/xfunky to ~/.xfunky to satrt-up some services

Others configirations files
===========================
	usr/share/xfunky/config/gtk-3.0 -> ~/config/gtk-3.0  ==> solve uneven color look at gnome-panel
	usr/share/xfunky/20-noveau.conf                      ==> config file for noveau drive using xrand in xorg
	usr/share/xfunky/xinitrc        -> ~/.xinitrc        ==> config file for startx
	
	# makes the mate-session calls the xfunky as its WM
	mateconftool-2 -s /desktop/mate/session/required_components/windowmanager xfunky --type string

	# Disable Caja desktop manager. Disable it, or xmobar will not appear (xmobar will stay behiden it)
	mateconftool-2 -s /apps/caja/preferences/show_desktop false --type boolean
        # also on org.gnome.desktop.background or org.mate.desktop.background
