
vim.opt_local.tabstop = 4
vim.opt_local.shiftwidth = 4
vim.opt_local.expandtab = true

vim.keymap.set('n', "vx", function() vim.call("<SID>expand_surround_tag()") end, { buffer = true, silent = true })

-- NOTE: this is an easy implementation
vim.cmd([[
function! s:expand_surround_tag() abort
  normal! g_hx_w
  let tag_name = expand('<cword>')
  normal! o</
  let @" = tag_name
  normal! pA>
endfunction
]])
