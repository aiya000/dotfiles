vim.api.nvim_create_autocmd({ 'BufNewFile', 'BufRead' }, {
  pattern = '*.tim',
  callback = function()
    vim.bo.filetype = 'timescript'
  end,
})
