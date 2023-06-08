set cursorline

nnoremap <buffer><silent> <CR> <Cmd>call ddu#ui#ff#do_action('itemAction')<CR>
nnoremap <buffer><silent> <Space> <Cmd>call ddu#ui#ff#do_action('toggleSelectItem')<CR>
nnoremap <buffer><silent> i <Cmd>call ddu#ui#ff#do_action('openFilterWindow')<CR>
nnoremap <buffer><silent> <C-l> <Cmd>call ddu#ui#ff#do_action('quit')<CR>
nnoremap <buffer><silent> <C-[> <Cmd>call ddu#ui#ff#do_action('quit')<CR>
