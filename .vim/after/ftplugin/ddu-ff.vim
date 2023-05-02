" call deoplete#custom#buffer_option('auto_complete', v:false)

" Avoids below error on calling ddu#ui#ff#do_action('itemAction')
"   [ddu] Error: Failed to call 'call' with ["denops#api#cmd",] ...
function s:open_file() abort
  execute 'normal' "vil\"zy:e\<Space>\<C-r>z\<CR>"
  " call ddu#ui#ff#do_action('itemAction')
endfunction

nnoremap <buffer><silent> <CR> <Cmd>call <SID>open_file()<CR>
" nnoremap <buffer><silent> <CR> <Cmd>call ddu#ui#ff#do_action('itemAction')<CR>
nnoremap <buffer><silent> <Space> <Cmd>call ddu#ui#ff#do_action('toggleSelectItem')<CR>
nnoremap <buffer><silent> i <Cmd>call ddu#ui#ff#do_action('openFilterWindow')<CR>
nnoremap <buffer><silent> <C-l> <Cmd>call ddu#ui#ff#do_action('quit')<CR>
nnoremap <buffer><silent> <C-[> <Cmd>call ddu#ui#ff#do_action('quit')<CR>
