vim.api.nvim_create_autocmd({ 'BufNewFile', 'BufRead' }, {
  pattern = '*.csx',
  callback = function()
    vim.bo.filetype = 'cs'
  end,
})
