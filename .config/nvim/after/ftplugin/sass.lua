vim.opt_local.tabstop = 2
vim.opt_local.shiftwidth = 2
vim.opt_local.expandtab = true

vim.keymap.set('v', 'i{', function()
  vim.cmd('call <SID>arrange_html_attribute_to_sass_attribute()')
end, { buffer = true, silent = true })

vim.cmd([[
function s:arrange_html_attribute_to_sass_attribute() range
  for line in range(a:firstline, a:lastline)
    execute 'normal!' (line .. 'G')
    execute 's/="/: /'
    execute 's/"$//'
    normal! _
    execute 'normal' "viw\<Plug>(operator-decamelize)"
    execute 's/_/-/g'
  endfor
endfunction
]])
