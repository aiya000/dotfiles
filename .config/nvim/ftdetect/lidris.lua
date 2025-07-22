vim.api.nvim_create_autocmd({ 'BufNewFile', 'BufRead' }, {
  pattern = '*.lidr',
  callback = function()
    vim.bo.filetype = 'lidris'
  end,
})
