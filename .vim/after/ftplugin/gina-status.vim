nnoremap <buffer><silent> <C-r> :<C-u>Gina status<CR>
nnoremap <buffer><silent> Q  :<C-u>bdelete!<CR>
nnoremap <buffer><silent> O  :<C-u>call <SID>open_file()<CR>
nnoremap <buffer><silent> cc :<C-u>q<CR>:tabnew<CR>:Gina commit --verbose<CR>:w<CR>:only<CR>ggO
nnoremap <buffer><silent> ca :<C-u>q<CR>:tabnew<CR>:Gina commit --verbose --amend<CR>gg0:only<CR>
nnoremap <buffer><silent> gf :<C-u>e <cfile><CR>
nnoremap <buffer><silent> g? :<C-u>call <SID>show_help_in_scratch_buffer()<CR>
nnoremap <buffer><silent> dd ^WviW"zy:<C-u>tabnew<CR>:Gina diff <C-r>z<CR>

function! s:open_file() abort
	if line('.') is 1
		" Open the first file
		normal! j
	endif
	let git_top_dir       = system('git rev-parse --show-toplevel')[:-2]  " [:-2] removes a line break
	let filename_from_top = join(split(getline('.'), ' ')[1:])  " [1:] removes a modifier
	let filename          = fnameescape(git_top_dir . '/' . filename_from_top)
	execute 'tabnew' filename
endfunction

function! s:show_help_in_scratch_buffer() abort
	let l:a = execute('normal ?')
	new
	setl buftype=nofile noreadonly modifiable
	put=l:a
	normal! ggdddd0
endfunction
