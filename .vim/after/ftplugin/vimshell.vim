let b:undo_ftplugin = 'setl ' . join([
\	'tabstop<',
\	'nolist<',
\	'wrap<',
\	'foldmethod<'
\])

setl tabstop=8
setl nolist
setl wrap
setl foldmethod=marker

" Override defaut keymappings
nnoremap <buffer> Q          gQ
nnoremap <buffer> <C-n>      gt
nnoremap <buffer> <C-p>      gT
nnoremap <buffer> <C-y>      <C-y>
nnoremap <buffer> q          <NOP>
nnoremap <buffer> <C-l>      <NOP>


nmap     <buffer> <C-w><C-c> <Plug>(vimshell_exit)
nmap     <buffer> <C-]>      <Plug>(vimshell_clear)
nmap     <buffer> gj         <Plug>(vimshell_next_prompt)
nmap     <buffer> gk         <Plug>(vimshell_previous_prompt)

inoremap <buffer> <C-l>      <Esc>
inoremap <buffer> <C-b>      <Left>
inoremap <buffer> <C-f>      <Right>
inoremap <buffer> <C-e>      <End>
inoremap <buffer> <C-n>      <Esc>gt
inoremap <buffer> <C-p>      <Esc>gT
inoremap <buffer> <C-d>      <Del>

imap     <buffer> <C-n>      <C-o><Plug>(vimshell_next_prompt)<End>
imap     <buffer> <C-p>      <C-o><Plug>(vimshell_previous_prompt)<End>
imap     <buffer> <C-]>      <Plug>(vimshell_clear)
imap     <buffer> <C-j>      <Plug>(vimshell_enter)
imap     <buffer> <C-k><C-p> <Plug>(vimshell_history_unite)

" These depends the command that is defined by vimrc
call vimshell#altercmd#define('tdirset', ':TDirSet')
call vimshell#altercmd#define('tdircd',  ':TDirCd')
call vimshell#altercmd#define('tdirpwd', ':TDirPwd')
call vimshell#altercmd#define('tab',     ':BufOpenNewTab')
call vimshell#altercmd#define('tabopen', ':BufMoveNewTab')
