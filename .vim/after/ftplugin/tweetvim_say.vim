let b:undo_ftplugin = 'setl ' . join([
\	'number<',
\	'relativenumber<',
\	'tabstop<',
\	'shiftwidth<',
\	'expandtab<'
\])

setl nonumber
setl norelativenumber
setl tabstop=2
setl shiftwidth=2
setl expandtab

" Avoid <C-j> to say
nnoremap <buffer> <C-j> o<Esc>
nnoremap <buffer> <localleader>a :<C-u>TweetVimSwitchAccount<Space>

inoremap <buffer> <C-i> <Tab>
inoremap <buffer> <C-b> <C-o>:TweetVimBitly<CR><C-r>+
