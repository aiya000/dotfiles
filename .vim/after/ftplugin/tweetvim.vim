let b:undo_ftplugin = 'setl ' . join([
\	'wrap<',
\	'cursorline<'
\])

setl wrap
setl cursorline

nnoremap <silent><buffer> s              :<C-u>TweetVimSay<CR>
nnoremap         <buffer> <C-a>          :<C-u>TweetVimSwitchAccount<Space>
nnoremap         <buffer> U              :<C-u>TweetVimUserTimeline<Space>
nnoremap <silent><buffer> Q              :<C-u>bdelete<CR>

nmap             <buffer> <localleader>R <Plug>(tweetvim_action_remove_status)
nmap             <buffer> <C-r>          <Plug>(tweetvim_action_reload)
nmap             <buffer> <CR>           <Plug>(openbrowser-open)
nmap             <buffer> [f             <Plug>(tweetvim_action_page_next)
nmap             <buffer> ]f             <Plug>(tweetvim_action_page_next)
nmap             <buffer> [b             <Plug>(tweetvim_action_page_previous)
nmap             <buffer> ]b             <Plug>(tweetvim_action_page_previous)
