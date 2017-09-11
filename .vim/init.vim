source ~/.vimrc
let s:Job = vital#vimrc#new().import('System.Job')

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

	" :terminal
	autocmd User MyNVimRc nnoremap <silent> <leader>v         :<C-u>call vimrc#keys#open_terminal_as('term-shell', 'vertical')<CR>
	autocmd User MyNVimRc nnoremap <silent> <leader><leader>v :<C-u>call vimrc#keys#open_terminal_as('term-shell', 'horizontal')<CR>
	autocmd User MyNVimRc nnoremap <silent> <leader>V         :<C-u>call vimrc#keys#open_terminal_as('term-shell', 'stay')<CR>
	autocmd User MyNVimRc nnoremap <silent> <leader><leader>V :<C-u>call vimrc#keys#open_terminal_as('term-shell', 'tabnew')<CR>

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

command! -bar Ghci  call nvimrc#open_stack_exec_ghci()
command! -bar Ghcie call nvimrc#open_stack_ghci()
CmdCnoreabbr CLisp terminal clisp
CmdCnoreabbr GCommit tabnew \| terminal git commit --verbose
CmdCnoreabbr GCAM tabnew \| terminal git commit --amend --verbose
CmdCnoreabbr GAP tabnew \| terminal git add -p

" }}}
" Others {{{

command! -bar NVimConfig    e ~/.config/nvim/init.vim
command! -bar NVimConfigTab tabnew ~/.config/nvim/init.vim

command! -bar Weechat call nvimrc#open_weechat()

command! -bar -nargs=? SnowtifyWatchStart call s:start_snowtify_watch(<q-args>)
command! -bar SnowtifyWatchStop call s:stop_snowtify_watch()

function! s:start_snowtify_watch(subcommand_or_empty) abort
    let subcommand = empty(a:subcommand_or_empty)
    \                ? 'test'
    \                : a:subcommand_or_empty
    let s:snowtify_watch_job = s:Job.start('watchexec -w . snowtify ' . subcommand)
endfunction

function! s:stop_snowtify_watch() abort
    if exists('s:snowtify_watch_job')
        if s:snowtify_watch_job.status() ==# 'run'
            call s:snowtify_watch_job.stop()
            echo 'snowtify watch is killed'
        else
            echo 'snowtify watch is alerady killed'
        endif
        unlet s:snowtify_watch_job
    else
        echo 'snowtify watch maybe not running'
    endif
endfunction

" }}}

doautocmd User MyNVimRc
