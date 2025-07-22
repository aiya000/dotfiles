vim.api.nvim_create_autocmd({ 'BufNewFile', 'BufRead' }, {
  pattern = '*.plist',
  callback = function()
    vim.bo.filetype = 'plist'
  end,
})
