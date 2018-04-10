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


"-----------------"
" Set vim options "
"-----------------"
" {{{

set inccommand=split

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
"--- ghcid ---" {{{

let g:ghcid_keep_open = v:true

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

    " disable vim-over keymaps
    autocmd User MyNVimRc nunmap :%s/
    autocmd User MyNVimRc nunmap :s/
    autocmd User MyNVimRc nnoremap <silent><expr> <C-k><C-s> ':%s/\m\C\<' . expand('<cword>') . '\>/'
    autocmd User MyNVimRc nnoremap <silent><expr> <C-k>s     ':%s/\m\C\<' . expand('<cword>') . '\>/' . expand('<cword>')
    autocmd User MyNVimRc vunmap :s/
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

" }}}
" Others {{{

command! -bar NVimConfig    e ~/.config/nvim/init.vim
command! -bar NVimConfigTab tabnew ~/.config/nvim/init.vim

command! -bar Weechat call nvimrc#open_weechat()

" }}}

doautocmd User MyNVimRc
