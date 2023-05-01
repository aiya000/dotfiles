function s:open_file() abort
  " NOTE: Avoids below error
  execute 'normal' "vil\"zy:e\<Space>\<C-r>z\<CR>"
  " [ddu] Error: Failed to call 'call' with ["denops#api#cmd",] ...
  " call ddu#ui#ff#do_action('itemAction')
endfunction

nnoremap <buffer><silent> <CR> <Cmd>call <SID>open_file()<CR>
" nnoremap <buffer><silent> <CR> <Cmd>call ddu#ui#ff#do_action('itemAction')<CR>
nnoremap <buffer><silent> <Space> <Cmd>call ddu#ui#ff#do_action('toggleSelectItem')<CR>
nnoremap <buffer><silent> i <Cmd>call ddu#ui#ff#do_action('openFilterWindow')<CR>
nnoremap <buffer><silent> <C-l> <Cmd>call ddu#ui#ff#do_action('quit')<CR>
nnoremap <buffer><silent> <C-[> <Cmd>call ddu#ui#ff#do_action('quit')<CR>
