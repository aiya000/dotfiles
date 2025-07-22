vim.api.nvim_create_autocmd({ 'BufNewFile', 'BufRead' }, {
  pattern = 'term://*weechat',
  callback = function()
    vim.bo.filetype = 'term-weechat'
  end,
})
