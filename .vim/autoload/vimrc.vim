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

" neobundle.vim cloner
function! vimrc#fetch_neobundle(bundledir)
	let l:neobundledir = a:bundledir . '/neobundle.vim'
	if executable('git')
		echo 'NeoBundle was not installed...'
		echo 'Installing NeoBundle.'
		execute '!git clone https://github.com/Shougo/neobundle.vim' l:neobundledir
	else
		call vimrc#echo_error('Sorry, You do not have git command.')
		call vimrc#echo_error('Cannot introduce NeoBundle.')
		throw 'FALIED: cloning neobundle.vim failed.'
	endif
endfunction
