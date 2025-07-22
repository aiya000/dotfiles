vim.opt_local.ts = 4
vim.opt_local.sw = 4
vim.opt_local.et = true
vim.opt_local.conceallevel = 0
vim.opt.commentstring = ' -- %s'

local augroup_FtpluginDhall = vim.api.nvim_create_augroup('FtpluginDhall', { clear = true })
vim.api.nvim_create_autocmd('BufWritePre', {
  group = augroup_FtpluginDhall,
  pattern = '*.dhall',
  callback = function()
    vim.cmd('Autoformat')
  end,
})
