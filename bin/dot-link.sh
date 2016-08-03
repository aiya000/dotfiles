#!/bin/sh -eu

echo '*** Warning ***'
echo 'This script force overwrite dotfiles that under $HOME'
echo '***************'
echo -n 'Do link dotfiles (y/n): '
read confirm
if [ "$confirm" != "y" ] ; then
	exit 0
fi

dotfiles_dir="${HOME}/.dotfiles"

echo "starting removing $HOME's dotfiles"
cat "${dotfiles_dir}/bin/target.txt" | xargs -I {} ln -s "${dotfiles_dir}/{}" "${HOME}/{}" \
	&& echo 'removing suceed.' \
	|| echo 'removing failed.'
echo

echo 'starting linkning'
cat "${dotfiles_dir}/bin/target.txt" | xargs -I {} ln -s "${dotfiles_dir}/{}" "${HOME}/{}" \
	&& echo 'linking suceed.' \
	|| echo 'linking failed.'
