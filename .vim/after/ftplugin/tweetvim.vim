let b:undo_ftplugin = 'setl ' . join([
    \ 'wrap<',
    \ 'cursorline<'
\])

setl wrap
setl cursorline

nmap <buffer> <C-a> <Plug>(tweetvim_action_page_next)
nmap <buffer> <C-r> <Plug>(tweetvim_action_reload)
nmap <buffer> <C-x> <Plug>(tweetvim_action_page_previous)
nmap <buffer> <localleader>R <Plug>(tweetvim_action_remove_status)
nmap <buffer> O <Plug>(tweetvim_action_favstar_browser)
nnoremap <buffer> U :<C-u>TweetVimUserTimeline<Space>
nnoremap <silent><buffer> <localleader>n :<C-u>split \| EditOverridden ~/.tmp/tweetvim_note.md \| set syntax=tweetvim_say<CR>
nnoremap <silent><buffer> Q :<C-u>bdelete<CR>
nnoremap <silent><buffer> s :<C-u>TweetVimSay<CR>
nnoremap <silent><buffer><nowait> t :<C-u>Denite unite:tweetvim<CR>
nnoremap <buffer><nowait> b b
nnoremap <buffer><nowait> f f
