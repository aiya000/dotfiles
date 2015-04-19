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

	autocmd User MyNVimRc tnoremap <C-l> <C-\><C-n>
	autocmd User MyNVimRc tnoremap <C-\><C-n> <Esc>
	autocmd User MyNVimRc tnoremap <C-]>      <C-l>

	"}}}
	" normal mode "{{{

	autocmd User MyNVimRc nnoremap <silent> <leader>v         :<C-u>vsp    \| terminal<CR>
	autocmd User MyNVimRc nnoremap <silent> <leader><leader>v :<C-u>sp     \| terminal<CR>
	autocmd User MyNVimRc nnoremap <silent> <leader>V         :<C-u>terminal<CR>
	autocmd User MyNVimRc nnoremap <silent> <leader><leader>V :<C-u>tabnew \| terminal<CR>

	"}}}
augroup END

" }}}


"-------------------------"
"      Command_Util       "
"-------------------------"
" Development "{{{

" GHCi
cnoreabbr Ghci    terminal ghci
cnoreabbr Sghci   sp     \| terminal ghci
cnoreabbr Vghci   vsp    \| terminal ghci
cnoreabbr GhciTab tabnew \| terminal ghci


" js
cnoreabbr Js      terminal js
cnoreabbr Sjs     sp     \| terminal js
cnoreabbr Vjs     vsp    \| terminal js
cnoreabbr JsTab   tabnew \| terminal js


" irb
cnoreabbr Irb     terminal irb
cnoreabbr Sirb    sp     \| terminal irb
cnoreabbr Virb    vsp    \| terminal irb
cnoreabbr IrbTab  tabnew \| terminal irb

" }}}
" Vim Utils {{{

command! -bar VimConfig     e ~/.vimrc
command! -bar VimConfigTab  tabnew ~/.vimrc
command! -bar NVimConfig    e $MYVIMRC
command! -bar NVimConfigTab tabnew $MYVIMRC

" }}}


doautocmd User MyNVimRc
