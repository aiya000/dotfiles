vim.api.nvim_create_autocmd({ 'BufNewFile', 'BufRead' }, {
  pattern = '*.scala.html',
  callback = function()
    vim.bo.filetype = 'play-template-scala'
  end,
})
