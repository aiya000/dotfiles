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


"------------------------"
"*** Plugin_Configure ***"
"------------------------"
"--- deoplete.nvim ---"{{{

let g:deoplete#enable_at_startup = 1

"}}}


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
	" Property based
	let s:SHELL_MODE = get(s:, 'SHELL_MODE', {
	\	'vimshell' : ':VimShell',
	\	'terminal' : ':terminal'
	\}) | lockvar! s:SHELL_MODE

	let s:start_shell_mode = get(s:, 'start_shell_mode', s:SHELL_MODE.terminal)
	\                        ==# s:SHELL_MODE.terminal ? s:SHELL_MODE.vimshell
	\                                                  : s:SHELL_MODE.terminal
	if s:start_shell_mode ==# s:SHELL_MODE.vimshell
		nnoremap <silent> <leader>v         :<C-u>vsp    \| terminal<CR>
		nnoremap <silent> <leader><leader>v :<C-u>sp     \| terminal<CR>
		nnoremap <silent> <leader>V         :<C-u>terminal<CR>
		nnoremap <silent> <leader><leader>V :<C-u>tabnew \| terminal<CR>
		echo 'shell mode ' . s:SHELL_MODE.terminal
	elseif s:start_shell_mode ==# s:SHELL_MODE.terminal
		nnoremap <silent> <leader>v         :<C-u>VimShell -split-command=vsp -toggle<CR>
		nnoremap <silent> <leader><leader>v :<C-u>VimShell -split-command=sp  -toggle<CR>
		nnoremap <silent> <leader>V         :<C-u>VimShellBufferDir -create<CR>
		nnoremap <silent> <leader><leader>V :<C-u>VimShell -split-command=tabnew -create<CR>
		echo 'shell mode ' . s:SHELL_MODE.vimshell
	endif

	unlockvar! s:SHELL_MODE
endfunction "}}}

" }}}
" By plugins {{{

"@Unchecked('')
augroup NeoKeyMapping
	" deoplete.nvim
	autocmd User MyVimRc inoremap <expr> <C-y> deoplete#mappings#cancel_popup() . '<C-y>'
	autocmd User MyVimRc inoremap <expr> <C-e> deoplete#mappings#cancel_popup() . '<C-e>'
	" Disable loaded neocomplete keymappings
	autocmd User MyVimRc inoremap <C-k><C-i> <NOP>
	autocmd User MyVimRc inoremap <CR>       <CR>
	autocmd User MyVimRc inoremap <Tab>      <Tab>
augroup END

" }}}
" Others {{{

augroup NeoKeyMapping
	" terminal mode "{{{

	" <C-l> is default Escape
	autocmd User MyNVimRc tnoremap <C-l>      <C-\><C-n>
	autocmd User MyNVimRc tnoremap <C-\><C-n> <Esc>
	autocmd User MyNVimRc tnoremap <C-]>      <C-l>

	"}}}
	" normal mode "{{{

	" Open new shell
	autocmd User MyNVimRc silent call s:toggle_start_shell_mode()
	autocmd User MyNVimRc nnoremap <silent> <C-\><C-v> :<C-u>call <SID>toggle_start_shell_mode()<CR>

	"}}}
augroup END

" }}}

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
