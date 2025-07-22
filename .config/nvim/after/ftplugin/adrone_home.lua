vim.opt_local.wrap = true
vim.opt_local.cursorline = true
vim.opt_local.number = false
vim.opt_local.relativenumber = false
vim.opt_local.list = false

vim.keymap.set('n', 'Q', function()
  vim.cmd('<C-u>bdelete')
end, { buffer = true, silent = true })

vim.cmd('nmap <buffer> <C-r> <Plug>(adrone_home_reload)')
vim.cmd('nmap <buffer> <C-x> <Plug>(adrone_home_future)')
vim.cmd('nmap <buffer> <C-a> <Plug>(adrone_home_past)')
vim.cmd('nmap <buffer> s     <Plug>(adrone_home_open_say)')
