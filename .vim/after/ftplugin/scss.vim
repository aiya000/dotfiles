let b:undo_ftplugin = 'setl ' . join([
  \ 'tabstop<',
  \ 'shiftwidth<',
  \ 'expandtab<',
\ ])

setl tabstop=4 shiftwidth=4 expandtab

vnoremap <buffer><silent> i{ :call <SID>arrange_html_attribute_to_sass_attribute()<CR>

function s:arrange_html_attribute_to_sass_attribute() range
  for line in range(a:firstline, a:lastline)
    execute 'normal!' (line .. 'G')
    execute 's/="/: /'
    execute 's/"$/;/'
    normal! _
    execute 'normal' "viw\<Plug>(operator-decamelize)"
    execute 's/_/-/g'
  endfor
endfunction
