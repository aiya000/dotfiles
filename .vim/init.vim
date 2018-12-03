source ~/.vimrc

"---------"
" Prepare "
"---------"
" Declare autocmd groups {{{

augroup InitVim
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


"-----------"
" Options "
"-----------"
"--- neovim --- {{{

set inccommand=split

" }}}
"--- gonvim ---" {{{

let g:gonvim_draw_statusline = 0
let g:gonvim_draw_tabline = 0

" }}}


"----------"
" augroups "
"----------"
" :terminal {{{

augroup InitVim
    autocmd TermOpen * setl nolist
augroup END

" }}}


"---------"
" Keymaps "
"---------"
" Override vimrc {{{

" Disable keymappings of vim-over, use 'inccommand' instead in neovim
nunmap :%s/
nunmap :s/
vunmap :s/
nnoremap <expr> <C-k><C-s> ':%s/\m\C\<' . expand('<cword>') . '\>/'
nnoremap <expr> <C-k>s ':%s/\m\C\<' . expand('<cword>') . '\>/' . expand('<cword>')

" }}}


"----------"
" Commands "
"----------"
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
