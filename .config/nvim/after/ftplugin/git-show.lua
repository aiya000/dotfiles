-- Load gitdiff.lua settings
vim.keymap.set('n', 'Q', function()
  vim.api.nvim_buf_delete(0, { force = true })
end, { buffer = true, silent = true })
