let b:undo_ftplugin = 'setl ' . join([
\   'expandtab<',
\   'shiftwidth<',
\   'tabstop<',
\])

setl tabstop=4 shiftwidth=4 expandtab

nnoremap <buffer> <leader><leader>R :<C-u>terminal yarn build<CR>
