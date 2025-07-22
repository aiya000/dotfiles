vim.api.nvim_create_autocmd({ 'BufNewFile', 'BufRead' }, {
  pattern = '*.elm',
  callback = function()
    vim.bo.filetype = 'elm'
  end,
})
