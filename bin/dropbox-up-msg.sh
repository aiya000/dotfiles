#!/bin/env sh
message=`echo $1 | sed 's;/;\&sl;g'`

# Prepare
if [ ! -d ~/.tmp ] ; then
	tmpdir_found=0
	mkdir ~/.tmp || ( echo "failed creating ~/.tmp" && exit 1 )
else
	tmpdir_found=1
fi

# Upload message file to dropbox with dropbox_uploader.sh
# dropbox_uploader.sh -> https://github.com/andreafabrizi/Dropbox-Uploader
if ( ! touch "${HOME}/.tmp/${message}" ) ; then
	echo "failed creating '~/.tmp/${message}'"
	exit 1
fi
dropbox_uploader.sh upload "${HOME}/.tmp/${message}" Room || exit 1
rm "${HOME}/.tmp/${message}" || ( echo "failed removing '~/.tmp/${message}'" && exit 1 )

# Clean
if [ $tmpdir_found -eq 0 ] ; then
	rmdir "${HOME}/.tmp" || ( echo "failed removing '~/.tmp/'" && exit 1 )
fi
