" If in visual_mode, set foldenable
" Else unset foldenable
let s:visual_fold_toggle = get(s:, 'visual_fold_toggle', 0)
function! vimrc#events#visual_fold_all() " {{{
	if mode() =~# "^[vV\<C-v>]"
		if !s:visual_fold_toggle && &foldenable
			set nofoldenable
			normal! zz
			let s:visual_fold_toggle = 1
		endif
	else
		if s:visual_fold_toggle
			set foldenable
			normal! zz
			let s:visual_fold_toggle = 0
		endif
	endif
endfunction " }}}
