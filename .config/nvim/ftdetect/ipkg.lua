vim.api.nvim_create_autocmd({ 'BufNewFile', 'BufRead' }, {
  pattern = '*.ipkg',
  callback = function()
    vim.bo.filetype = 'ipkg'
  end,
})
