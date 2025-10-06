vim.opt_local.tabstop = 4
vim.opt_local.shiftwidth = 4
vim.opt_local.textwidth = 0

vim.schedule(function()
  vim.api.nvim_win_set_cursor(0, { 1, 0 })
end, 0)
