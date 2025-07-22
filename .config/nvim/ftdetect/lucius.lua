vim.api.nvim_create_autocmd({ 'BufNewFile', 'BufRead' }, {
  pattern = '*.lucius',
  callback = function()
    vim.bo.filetype = 'lucius'
  end,
})
