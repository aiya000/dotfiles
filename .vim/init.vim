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
" nvim-hs {{{

call remote#host#Register('haskell', "*.l\?hs", {name ->
    \ jobstart(
        \ [
            \ 'stack',
            \ 'exec',
            \ 'nvim-hs',
            \ name.name,
        \ ], {
            \ 'rpc' : v:true,
            \ 'cwd' : expand('$HOME') . '/.config/nvim/my-nvim-hs',
        \ }
    \ )
\ })

let hc = remote#host#Require('haskell')

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
" Override vimrc {{{

" Disable keymappings of neocomplete, use deoplete.nvim instead in neovim
inoremap <CR> <CR>
inoremap <Tab> <Tab>
inoremap <expr> <C-y> deoplete#mappings#cancel_popup() . '<C-y>'
inoremap <expr> <C-e> deoplete#mappings#cancel_popup() . '<C-e>'
imap <C-k><C-i> <C-o>:call deoplete#toggle()<CR>

" Disable keymappings of vim-over, use 'inccommand' instead in neovim
nunmap :%s/
nunmap :s/
vunmap :s/
nnoremap <expr> <C-k><C-s> ':%s/\m\C\<' . expand('<cword>') . '\>/'
nnoremap <expr> <C-k>s ':%s/\m\C\<' . expand('<cword>') . '\>/' . expand('<cword>')

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
