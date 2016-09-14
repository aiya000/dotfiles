#!/bin/sh
result=$(dropbox-cli status)
if [ "$result" != "Dropbox isn't running!" ] ; then
	echo "[${result}] "
fi
