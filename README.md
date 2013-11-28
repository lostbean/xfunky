Dual Head Config
================

Use either twinview (NVidia) or Xrandr. If you are using Xrandr then Xinerama (libXinerama) should be available.

Build Tips (Debian)
==================

Use the command "debuild -us -uc" to build and pack the debian package [http://wiki.debian.org/IntroDebianPackaging]
Compiling without xinerama will make xmonad see the dual head screen as one big screen (extended mode).

Building Tips (Gentoo)
=====================

Run to command "ebuild xfunky-1.0.ebuild manifest clean package" where the ".ebuild" is to test the package e run "ebuild xfunky-1.0.ebuild manifest clean merge" for installing.

Configutarion for KDE4
======================

You should choose "XFunky" as window manager in: System Settings -> Default Applications -> Window Manager

Configutarion for Gnome-3.x (old version)
=========================================

Using gnome-tweak-tool, set off the desktop managment by the file manager in order to focus the Workspace whenever the mouse moves to an empty one.
Also:
	# solve uneven color look at gnome-panel
	usr/share/xfunky/config/gtk-3.0 -> ~/config/gtk-3.0

	# makes the mate-session calls the xfunky as its WM
	mateconftool-2 -s /desktop/mate/session/required_components/windowmanager xfunky --type string

	# Disable Caja desktop manager. Disable it, or xmobar will not appear (xmobar will stay behiden it)
	mateconftool-2 -s /apps/caja/preferences/show_desktop false --type boolean
        # also on org.gnome.desktop.background or org.mate.desktop.background


