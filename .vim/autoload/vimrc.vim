" Clone dein.vim to target dir
function! vimrc#fetch_dein(install_dirname)
	if executable('git')
		echo 'dein.vim was not installed yet.'
		echo 'Installing dein.vim now.'
		execute '!git clone https://github.com/Shougo/dein.vim' a:install_dirname
	else
		call vimrc#echo_error('Sorry, You do not have git command.')
		call vimrc#echo_error('I cannot introduce dein.vim.')
		throw 'FALIED: cloning deim.vim failed.'
	endif
endfunction
