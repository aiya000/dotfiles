vim.api.nvim_create_autocmd({ 'BufNewFile', 'BufRead' }, {
  pattern = '*.adlog',
  callback = function()
    vim.bo.filetype = 'adlog'
  end,
})
