vim.api.nvim_create_autocmd({ 'BufNewFile', 'BufRead' }, {
  pattern = '*.toml',
  callback = function()
    vim.bo.filetype = 'toml'
  end,
})
