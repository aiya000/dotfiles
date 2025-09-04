vim.opt.commentstring = ' /*%s*/'
vim.opt_local.tabstop = 2
vim.opt_local.shiftwidth = 2
vim.opt_local.expandtab = true
vim.opt.errorformat = '[%t%.%#] %f:%l:%m'

vim.keymap.set('n', 'w', function()
  vim.cmd('<C-u>QuickfixRunSbtCompileWatch')
end, { buffer = true, silent = true })
vim.keymap.set('n', 'W', function()
  vim.cmd('<C-u>QuickfixStopSbtCompileWatch')
end, { buffer = true, silent = true })
