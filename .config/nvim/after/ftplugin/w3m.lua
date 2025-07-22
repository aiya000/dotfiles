vim.opt_local.wrapscan = true

vim.cmd('nmap <buffer> H     <Plug>(w3m-back)')
vim.cmd('nmap <buffer> L     <Plug>(w3m-forward)')
vim.cmd('nmap <buffer> t     <Plug>(w3m-shift-click)')
vim.cmd('nmap <buffer> i     <Plug>(w3m-address-bar)')
vim.cmd('nmap <buffer> <C-i> <Plug>(w3m-next-link)')
vim.cmd('nmap <buffer> <C-o> <Plug>(w3m-prev-link)')

vim.keymap.set('n', 'r', function()
  vim.cmd('<C-u>W3mShowExtenalBrowser')
end, { buffer = true, silent = true })
