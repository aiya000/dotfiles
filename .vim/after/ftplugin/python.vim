let b:undo_ftplugin = 'setl ' . join([
\   'tabstop<',
\   'shiftwidth<',
\   'expandtab<',
\])

setl ts=4 sw=4 et

nnoremap <buffer><silent> <leader><leader>R :<C-u>call vimrc#open_terminal_as('term-nosetests', 'horizontal', './.venv/bin/nosetests')<CR>
nnoremap <buffer><silent> <leader>o :<C-u>vsp \| IPyRepl<CR>
