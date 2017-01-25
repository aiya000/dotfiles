#!/bin/sh

# Use this temporarily if you get into a bug of xmonad keymode switching

if [ -f "/tmp/xmonad-keymode-hhkb" ] ; then
	rm /tmp/xmonad-keymode-hhkb
else
	touch /tmp/xmonad-keymode-hhkb
fi

xmonad-config --restart
