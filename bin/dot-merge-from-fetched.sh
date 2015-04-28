#!/bin/sh

DIFF_DIR1=~/.dotfiles
DIFF_DIR2=~/.tmp/.dotfiles

files=`find $DIFF_DIR2 -type f | awk -F'.dotfiles/' '{print $2}' | grep -v '\.git'`


#@Incomplete('funny function diff')
# View difference
echo '- - - Difference List - - -'
tmp_file1=`mktemp`
tmp_file2=`mktemp`

find $DIFF_DIR1 | awk -F'.dotfiles/' '{print $2}' | grep -v '\.git' > $tmp_file1
find $DIFF_DIR2 | awk -F'.dotfiles/' '{print $2}' | grep -v '\.git' > $tmp_file2

diff $tmp_file1 $tmp_file2

rm $tmp_file1
rm $tmp_file2
echo '- - - - - - - - - - - - - -'


# Execute diff files
for file in $files ; do
	file1=$DIFF_DIR1/$file
	file2=$DIFF_DIR2/$file

	# detected difference
	if [ -f $file2 -a -n "`diff $file1 $file2`" ] ; then

		# confirm edit
		read -p "Edit >> ${file} ? (y/n) (y): " confirm

		if [ "$confirm" = "yes" -o "$confirm" = "y" -o "$confirm" = "" ] ; then
			vimdiff $file1 $file2
		fi

		echo
	fi
done
