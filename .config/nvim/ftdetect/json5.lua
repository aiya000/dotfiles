vim.api.nvim_create_autocmd({ 'BufNewFile', 'BufRead' }, {
  pattern = { '*.json5', '.textlintrc' },
  callback = function()
    vim.bo.filetype = 'json5'
  end,
})
