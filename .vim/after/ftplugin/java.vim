let b:undo_ftplugin = 'setl ' . join([
    \ 'commentstring<'
\])
let &commentstring = ' /*%s*/'

nnoremap <buffer><silent> <localleader><localleader>r :<C-u>call vimrc#plugins#quickrun_gradle_build()<CR>
nnoremap <buffer><silent> <localleader><localleader>R :<C-u>call vimrc#open_terminal_as('term-gradle', 'horizontal', "bash -c 'cd $(git rev-parse --show-toplevel) && gradle build'")<CR>
