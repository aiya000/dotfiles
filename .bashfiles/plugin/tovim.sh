#!/bin/bash -e
# see http://vim-jp.org/blog/2015/10/15/tovim-on-shell-command-pipes.html

tovim () {
	TOVIMTMP="/tmp/tovim_tmp_`date +'%Y-%m-%d_%H-%M-%S'`"
	trap 'rm $TOVIMTMP' ERR

	cat > $TOVIMTMP
	vim $TOVIMTMP < /dev/tty > /dev/tty
	cat $TOVIMTMP
	rm $TOVIMTMP
}
