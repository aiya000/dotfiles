vim.api.nvim_create_autocmd({ 'BufNewFile', 'BufRead' }, {
  pattern = 'conky.conf',
  callback = function()
    vim.bo.filetype = 'lua'
  end,
})
