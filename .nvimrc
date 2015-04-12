source ~/.vimrc

"-------------------
"--  Recipe Menu  --
"-------------------
" -- Initialize
" -- Event_Method
" -- Key_Mapping
" ---



"-------------------------"
"       Initialize        "
"-------------------------"
" autocmd Groups {{{

augroup NeoEvent
	autocmd!
augroup END

augroup NeoKeyMapping
	autocmd!
augroup END

" }}}


"-------------------------"
"      Event_Method       "
"-------------------------"
" :terminal {{{

augroup NeoEvent
	autocmd TermOpen * setl nolist
augroup END

" }}}


"-------------------------"
"       Key_Mapping       "
"-------------------------"
" Global {{{

augroup NeoKeyMapping
	" terminal mode "{{{

	autocmd User MyNVimRc tnoremap <silent> <C-l> <C-\><C-n>
	autocmd User MyNVimRc tnoremap <silent> <C-\><C-n> <Esc>

	"}}}
	" normal mode "{{{

	autocmd User MyNVimRc nnoremap <silent> <leader>v         :<C-u>vsp    \| terminal<CR>
	autocmd User MyNVimRc nnoremap <silent> <leader><leader>v :<C-u>sp     \| terminal<CR>
	autocmd User MyNVimRc nnoremap <silent> <leader>V         :<C-u>terminal<CR>
	autocmd User MyNVimRc nnoremap <silent> <leader><leader>V :<C-u>tabnew \| terminal<CR>

	"}}}
augroup END

" }}}


doautocmd User MyNVimRc
