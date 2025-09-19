if vim.bo.readonly then
  -- Settings for read-only help files
  vim.opt_local.list = false
  vim.opt_local.number = false
  vim.opt_local.relativenumber = false
  vim.keymap.set('n', 'Q', '<Cmd>helpclose<CR>', { buffer = true, silent = true })
else
  -- Settings for editable help files
  vim.opt_local.tabstop = 2
  vim.opt_local.shiftwidth = 2
  vim.opt_local.expandtab = true
  vim.opt_local.conceallevel = 1
  vim.keymap.set('i', '<Tab>', '	', { buffer = true })
end
