vim.api.nvim_create_autocmd({ 'BufNewFile', 'BufRead' }, {
  pattern = '*.julius',
  callback = function()
    vim.bo.filetype = 'julius'
  end,
})
