vim.api.nvim_create_autocmd({ 'BufNewFile', 'BufRead' }, {
  pattern = 'Podfile',
  callback = function()
    vim.bo.filetype = 'ruby'
  end,
})
