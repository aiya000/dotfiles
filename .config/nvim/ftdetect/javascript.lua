vim.api.nvim_create_autocmd({ 'BufNewFile', 'BufRead' }, {
  pattern = '*.es6',
  callback = function()
    vim.bo.filetype = 'javascript'
  end,
})
