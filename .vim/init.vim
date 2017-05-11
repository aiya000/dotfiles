source ~/.vimrc

"--------------"
" Starting up "
"--------------"
" Define global values {{{

" For the exceptions
let $NVIM_PYTHON_LOG_FILE = g:vimrc['vim_home'] . '/.log/nvim_python.log'

" }}}
" Declare autocmd groups {{{

augroup NeoEvent
	autocmd!
augroup END

augroup NeoKeyMapping
	autocmd!
augroup END

" }}}


"---------------------"
" Configurate plugins "
"---------------------"
"--- deoplete.nvim ---"{{{

let g:deoplete#enable_at_startup = 1

"}}}
"--- vimdoc-ja ---" {{{

"@Bug("Didn't work")
" vimdoc-ja for vim only (not for neovim)
call dein#disable('vimdoc-ja')

"}}}


"---------------------"
" Set augroup details "
"---------------------"
" :terminal {{{

augroup NeoEvent
	autocmd TermOpen * setl nolist
augroup END

" }}}


"--------------------"
" Define keymappings "
"--------------------"
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
	" Turn off the neocomplete keymaps
	autocmd User MyNVimRc inoremap <CR> <CR>
	autocmd User MyNVimRc inoremap <Tab> <Tab>

	" deoplete.nvim
	autocmd User MyNVimRc inoremap <expr> <C-y> deoplete#mappings#cancel_popup() . '<C-y>'
	autocmd User MyNVimRc inoremap <expr> <C-e> deoplete#mappings#cancel_popup() . '<C-e>'
	autocmd User MyNVimRc imap <C-k><C-i> <C-o>:call deoplete#toggle()<CR>
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
	autocmd User MyNVimRc tnoremap <leader><C-n> <C-\><C-n>

	"}}}
	" normal mode "{{{

	" Open new shell
	autocmd User MyNVimRc silent call s:toggle_start_shell_mode()
	autocmd User MyNVimRc nnoremap <silent> <C-\><C-v> :<C-u>call <SID>toggle_start_shell_mode()<CR>

	"}}}
augroup END

" }}}

" }}}


"-----------------"
" Manage commands "
"-----------------"
" Overwrite defined commands {{{

command! -bar VimConfig    e ~/.vimrc
command! -bar VimConfigTab tabnew ~/.vimrc
command! -bar Reload       so ~/.config/nvim/init.vim

CmdCnoreabbr Ghci terminal stack exec ghci
CmdCnoreabbr CLisp terminal clisp
CmdCnoreabbr GCommit tabnew \| terminal git commit --verbose

" }}}
" Others {{{

command! -bar NVimConfig    e ~/.config/nvim/init.vim
command! -bar NVimConfigTab tabnew ~/.config/nvim/init.vim

command! -bar Weechat call nvimrc#open_weechat()

" Git
CmdCnoreabbr GAP tabnew \| terminal git add -p

" }}}

doautocmd User MyNVimRc
