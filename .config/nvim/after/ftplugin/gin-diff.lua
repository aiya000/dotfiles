vim.opt_local.list = false
vim.opt_local.tabstop = 8

vim.keymap.set('n', 'Q', function()
  vim.cmd('bdelete!')
end, { buffer = true, silent = true })

vim.cmd("\\  if &filetype ==# 'gin-diff'")
vim.cmd('\\| IndentGuidesDisable')
vim.cmd('\\| endif')
