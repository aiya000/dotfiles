let b:undo_ftplugin = 'setl ' . join([
  \ 'nonumber<',
  \ 'norelativenumber<',
  \ 'nolist<',
\])

setl nonumber norelativenumber nolist

nnoremap <buffer><silent> <C-r> :<C-u>call vimrc#open_terminal_as('term-gradle', 'horizontal', "bash -c 'cd $(git rev-parse --show-toplevel) && gradle build'")<CR><C-w>p:quit<CR><C-w>p
