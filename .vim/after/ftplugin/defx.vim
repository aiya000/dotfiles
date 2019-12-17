let b:undo_ftplugin = 'setl ' . join([
  \ 'nolist<'
\ ])

setl nolist

nnoremap <buffer> Q :<C-u>quit<CR>
nnoremap <buffer> <leader>e :<C-u>quit<CR>

nnoremap <buffer><expr> % defx#do_action('new_file')
nnoremap <buffer><expr> d defx#do_action('new_directory')
nnoremap <buffer><expr><silent> H defx#do_action('cd', ['..'])
nnoremap <buffer><expr><silent> D defx#do_action('remove')
nnoremap <buffer><expr><silent> o defx#do_action('open')
nnoremap <buffer><expr><silent> i defx#do_action('open_or_close_tree')
nnoremap <buffer><expr><silent> <CR> defx#do_action('open')
nnoremap <buffer><expr><silent> <C-j> defx#do_action('open')
