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
" Environment {{{

" For the exceptions
let $NVIM_PYTHON_LOG_FILE = g:vimrc['vim_home'] . '/.log/nvim_python.log'

" }}}
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
	" Base properties
	let l:SHELL_MODE = {
	\	'vimshell' : ':VimShell',
	\	'terminal' : ':terminal'
	\} | lockvar! l:SHELL_MODE

	" Toggle values
	let s:start_shell_mode = get(s:, 'start_shell_mode', l:SHELL_MODE.terminal)
	\                        ==# l:SHELL_MODE.terminal ? l:SHELL_MODE.vimshell
	\                                                  : l:SHELL_MODE.terminal

	" Toggle keymappings
	if s:start_shell_mode ==# l:SHELL_MODE.vimshell
		nnoremap <silent> <leader>v         :<C-u>vsp    \| terminal<CR>
		nnoremap <silent> <leader><leader>v :<C-u>sp     \| terminal<CR>
		nnoremap <silent> <leader>V         :<C-u>terminal<CR>
		nnoremap <silent> <leader><leader>V :<C-u>tabnew \| terminal<CR>
		echo 'shell mode ' . l:SHELL_MODE.terminal
	elseif s:start_shell_mode ==# l:SHELL_MODE.terminal
		nnoremap <silent> <leader>v         :<C-u>VimShell -split-command=vsp -toggle<CR>
		nnoremap <silent> <leader><leader>v :<C-u>VimShell -split-command=sp  -toggle<CR>
		nnoremap <silent> <leader>V         :<C-u>VimShellBufferDir -create<CR>
		nnoremap <silent> <leader><leader>V :<C-u>VimShell -split-command=tabnew -create<CR>
		echo 'shell mode ' . l:SHELL_MODE.vimshell
	endif
endfunction "}}}

" }}}
" Override vimrc {{{

augroup NeoKeyMapping
	" Disable loaded neocomplete keymappings
	autocmd User MyNVimRc inoremap <C-k><C-i> <NOP>
	autocmd User MyNVimRc inoremap <CR>       <CR>
	autocmd User MyNVimRc inoremap <Tab>      <Tab>

	" deoplete.nvim
	autocmd User MyNVimRc inoremap <expr> <C-y> deoplete#mappings#cancel_popup() . '<C-y>'
	autocmd User MyNVimRc inoremap <expr> <C-e> deoplete#mappings#cancel_popup() . '<C-e>'

	" aref-web.vim
	autocmd User MyNVimRc nnoremap <leader>K :<C-u>vsp \| VimRunDo Aref weblio <C-r>=expand('<cword>')<CR><CR>
	autocmd User MyNVimRc nnoremap <leader>S :<C-u>vsp \| VimRunDo Aref stackage <C-r>=expand('<cword>')<CR><CR>
	autocmd User MyNVimRc vnoremap <leader>K "zy:<C-u>vsp \| VimRunDo Aref weblio <C-r>z<CR>
	autocmd User MyNVimRc vnoremap <leader>S "zy:<C-u>vsp \| VimRunDo Aref stackage <C-r>z<CR>
augroup END

" }}}
" Others {{{

augroup NeoKeyMapping
	" terminal mode "{{{

	" Default,
	" <C-\><C-n> is sending Esc signal to NeoVim,
	" and <Esc> is sending Esc signal to innert program
	autocmd User MyNVimRc tnoremap <C-l>      <C-\><C-n>
	autocmd User MyNVimRc tnoremap <C-\><C-n> <Esc>
	autocmd User MyNVimRc tnoremap <C-[>      <Esc>
	autocmd User MyNVimRc tnoremap <C-]>      <C-l>

	"}}}
	" normal mode "{{{

	" Open new shell
	autocmd User MyNVimRc silent call s:toggle_start_shell_mode()
	autocmd User MyNVimRc nnoremap <silent> <C-\><C-v> :<C-u>call <SID>toggle_start_shell_mode()<CR>
	autocmd User MyNVimRc nnoremap <silent> <C-\><C-l> :<C-u>call <SID>toggle_esc_keys_target()<CR>

	"}}}
augroup END

" }}}

" }}}


"-------------------------"
"      Command_Util       "
"-------------------------"
" :terminal shortcut "{{{

CmdCnoreabbr Weechat terminal weechat

" }}}
" NeoVim Utils {{{

command! -bar VimConfig     e ~/.vimrc
command! -bar VimConfigTab  tabnew ~/.vimrc
"TODO: use $XDG_CONFIG_HOME (?)
command! -bar NVimConfig    e ~/.config/nvim/init.vim
command! -bar NVimConfigTab tabnew ~/.config/nvim/init.vim

" Override definition :Reload from .vimrc
command! -bar Reload so ~/.config/nvim/init.vim

" }}}
" Override vimrc {{{

" aref-web.vim
CmdCnoreabbr Weblio   VimRunDo Aref weblio
CmdCnoreabbr Stackage VimRunDo Aref stackage
CmdCnoreabbr Hoogle   VimRunDo Aref hoogle

" }}}

doautocmd User MyNVimRc
