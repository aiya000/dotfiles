let b:undo_ftplugin = 'setl ' . join([
    \ 'wrap<',
    \ 'cursorline<'
\])

setl wrap
setl cursorline

nnoremap <silent><buffer> s              :<C-u>TweetVimSay<CR>
nnoremap         <buffer> U              :<C-u>TweetVimUserTimeline<Space>
nnoremap <silent><buffer> Q              :<C-u>bdelete<CR>
nnoremap <silent><buffer> <localleader>n :<C-u>split \| EditOverridden ~/.tmp/tweetvim_note.md \| set syntax=tweetvim_say<CR>

nmap             <buffer> <localleader>R <Plug>(tweetvim_action_remove_status)
nmap             <buffer> <C-r>          <Plug>(tweetvim_action_reload)
nmap             <buffer> <C-a>          <Plug>(tweetvim_action_page_next)
nmap             <buffer> <C-x>          <Plug>(tweetvim_action_page_previous)
nmap             <buffer> O              <Plug>(tweetvim_action_favstar_browser)

nunmap           <buffer> ff
nunmap           <buffer> bb
