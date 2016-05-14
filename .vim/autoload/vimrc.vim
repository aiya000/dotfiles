" Common utils

"TODO: Implement with multi arguments
" Run system command by vimproc or vim default
function! vimrc#system(cmd)
	"@Incomplete('vimproc#system was not executed in this script, refer to vital.vim')
	if g:vimrc['is_windows']
		silent execute '!start' a:cmd
	elseif exists('*vimproc#system')
		return vimproc#system(a:cmd)
	else
		return system(a:cmd)
	endif
endfunction

" Show error message
function! vimrc#echo_error(msg)
	echohl Error
	echo a:msg
	echohl None
endfunction

"#-=- -=- -=- -=- -=- -=- -=- -=- -=-#"
" Initializer

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
