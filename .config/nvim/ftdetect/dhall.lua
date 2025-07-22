vim.api.nvim_create_autocmd({ 'BufNewFile', 'BufRead' }, {
  pattern = '*.dhall',
  callback = function()
    vim.bo.filetype = 'dhall'
  end,
})
