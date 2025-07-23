-- General log filetype
vim.api.nvim_create_autocmd({ 'BufNewFile', 'BufRead' }, {
  pattern = '*.log',
  callback = function()
    vim.bo.filetype = 'log'
  end,
})
