function! s:show_help_in_scratch_buffer() abort
	let l:a = execute('normal ?')
	new
	setl buftype=nofile noreadonly modifiable
	put=l:a
	normal! ggdddd0
endfunction

nnoremap <buffer><silent> Q  :<C-u>bdelete!<CR>
nnoremap <buffer><silent> cc :<C-u>Gina commit --verbose<CR>gg0
nnoremap <buffer><silent> ca :<C-u>Gina commit --verbose --amend<CR>gg0
nnoremap <buffer><silent> gf :<C-u>e <cfile><CR>
nnoremap <buffer><silent> g? :<C-u>call <SID>show_help_in_scratch_buffer()<CR>
