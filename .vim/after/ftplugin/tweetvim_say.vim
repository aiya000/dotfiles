if bufname('%') ==# 'tweetvim_say'
  resize 10
endif

let b:undo_ftplugin = 'setl ' . join([
  \ 'number<',
  \ 'relativenumber<',
  \ 'tabstop<',
  \ 'shiftwidth<',
  \ 'expandtab<'
\ ])

setl nonumber
setl norelativenumber
setl tabstop=2
setl shiftwidth=2
setl expandtab

" Avoid <C-j> to say
nnoremap <buffer> <C-j> o<Esc>
nnoremap <buffer> <localleader>a :<C-u>TweetVimSwitchAccount<Space>
nmap <buffer> <C-o> <Plug>(tweetvim_say_show_history)
nmap <buffer> <C-i> <Plug>(tweetvim_say_post_buffer)

inoremap <buffer> <C-i> <Tab>
imap <buffer> <C-b> <C-o>:TweetVimBitly<CR><C-r>+

" Recover my <C-s>
inoremap <silent><expr><buffer> <C-s> neosnippet#mappings#expand_or_jump_impl()
