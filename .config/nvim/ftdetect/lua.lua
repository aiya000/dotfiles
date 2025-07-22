vim.api.nvim_create_autocmd({ 'BufNewFile', 'BufRead' }, {
  pattern = { '*.lua', 'conky.conf' },
  callback = function()
    vim.bo.filetype = 'lua'
  end,
})
