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
nmap             <buffer> >              <Plug>(tweetvim_action_page_next)
nmap             <buffer> <              <Plug>(tweetvim_action_page_previous)

nunmap           <buffer> ff
nunmap           <buffer> bb

"@Incomplete('? reset when execute :colorscheme')
if has('win32unix')
	highlight CursorLine term=standout cterm=standout ctermbg=Blue guibg=Grey40
else
	highlight CursorLine term=standout cterm=standout ctermbg=Black guibg=Grey40
endif
