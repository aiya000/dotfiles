let s:V      = vital#vimrc#new()
let s:String = s:V.import('Data.String')

nmap <buffer> dd <Plug>(gina-diff)
" gH is defined in vimrc
nmap <buffer> O  gH<Plug>(gina-edit)

nnoremap <buffer><silent> ? :<C-u>call <SID>show_help_in_scratch_buffer()<CR>
nnoremap <buffer><silent> <C-r> :<C-u>Gina status<CR>
nnoremap <buffer><silent> Q  :<C-u>bdelete!<CR>
nnoremap <buffer><silent> cc :<C-u>q<CR>:tabnew<CR>:Gina commit --verbose<CR>:w<CR>:only<CR>ggO
nnoremap <buffer><silent> ca :<C-u>q<CR>:tabnew<CR>:Gina commit --verbose --amend<CR>gg0:only<CR>
nnoremap <buffer><silent> gf :<C-u>e <cfile><CR>

function! s:show_help_in_scratch_buffer() abort
	let l:a = execute('normal ' . "\<Plug>(gina-builtin-help)")
	new
	setl buftype=nofile noreadonly modifiable ft=scratch
	put=l:a
	normal! ggdddd0
endfunction
