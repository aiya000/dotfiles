-- This overwrites filetype yacc
vim.api.nvim_create_autocmd({ 'BufNewFile', 'BufRead' }, {
  pattern = '*.y',
  callback = function()
    vim.bo.filetype = 'happy'
  end,
})
