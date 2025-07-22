vim.api.nvim_create_autocmd({ 'BufNewFile', 'BufRead' }, {
  pattern = '*.xaml',
  callback = function()
    vim.bo.filetype = 'xaml'
  end,
})
