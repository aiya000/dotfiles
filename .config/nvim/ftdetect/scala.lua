vim.api.nvim_create_autocmd({ 'BufNewFile', 'BufRead' }, {
  pattern = '*.scala',
  callback = function()
    vim.bo.filetype = 'scala'
  end,
})

vim.api.nvim_create_autocmd({ 'BufNewFile', 'BufRead' }, {
  pattern = '*.sbt',
  callback = function()
    vim.bo.filetype = 'sbt.scala'
  end,
})
