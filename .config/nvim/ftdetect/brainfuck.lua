vim.api.nvim_create_autocmd({ 'BufNewFile', 'BufRead' }, {
  pattern = '*.bf',
  callback = function()
    vim.bo.filetype = 'brainfuck'
  end,
})
