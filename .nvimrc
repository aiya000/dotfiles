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
" unset some plugins {{{

"NeoBundleDisable 'Shougo/neocomplete.vim'

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

" Prepare functions {{{

" Toggle keymapping <leader>V (and etc) to :terminal or vimshell
function! s:toggle_start_shell_mode() "{{{
	let s:start_shell_mode = get(s:, 'start_shell_mode', ':VimShell')
	\                        ==# ':VimShell' ? ':terminal'
	\                                        : ':VimShell'
	if s:start_shell_mode ==# ':terminal'
		nnoremap <silent> <leader>v         :<C-u>VimShell -split-command=vsp -toggle<CR>
		nnoremap <silent> <leader><leader>v :<C-u>VimShell -split-command=sp  -toggle<CR>
		nnoremap <silent> <leader>V         :<C-u>VimShellBufferDir -create<CR>
		nnoremap <silent> <leader><leader>V :<C-u>VimShell -split-command=tabnew -create<CR>
		echo 'shell mode :VimShell'
	elseif s:start_shell_mode ==# ':VimShell'
		nnoremap <silent> <leader>v         :<C-u>vsp    \| terminal<CR>
		nnoremap <silent> <leader><leader>v :<C-u>sp     \| terminal<CR>
		nnoremap <silent> <leader>V         :<C-u>terminal<CR>
		nnoremap <silent> <leader><leader>V :<C-u>tabnew \| terminal<CR>
		echo 'shell mode :terminal'
	endif
endfunction "}}}

" }}}

augroup NeoKeyMapping
	" terminal mode "{{{

	" <C-l> is default Escape
	autocmd User MyNVimRc tnoremap <C-l>      <C-\><C-n>
	autocmd User MyNVimRc tnoremap <C-\><C-n> <Esc>
	autocmd User MyNVimRc tnoremap <C-]>      <C-l>

	"}}}
	" normal mode "{{{

	" Start shell
	autocmd User MyNVimRc nnoremap <silent> <leader>v         :<C-u>vsp    \| terminal<CR>
	autocmd User MyNVimRc nnoremap <silent> <leader><leader>v :<C-u>sp     \| terminal<CR>
	autocmd User MyNVimRc nnoremap <silent> <leader>V         :<C-u>terminal<CR>
	autocmd User MyNVimRc nnoremap <silent> <leader><leader>V :<C-u>tabnew \| terminal<CR>
	autocmd User MyNVimRc nnoremap <silent> <C-\><C-v>        :<C-u>call <SID>toggle_start_shell_mode()<CR>

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

" stack GHCi
cnoreabbr GhciSt    terminal stack ghci
cnoreabbr SghciSt   sp     \| terminal stack ghci
cnoreabbr VghciSt   vsp    \| terminal stack ghci
cnoreabbr GhciStTab tabnew \| terminal stack ghci


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
command! -bar -nargs=* -complete=file Vim    terminal vim <args>
command! -bar -nargs=* -complete=file VimTab tabnew | terminal vim <args>

" }}}


doautocmd User MyNVimRc
