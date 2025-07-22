-- Unity
vim.api.nvim_create_autocmd({ 'BufNewFile', 'BufRead' }, {
  pattern = { '*.unity', '*.meta', '*.controller', '*.anim' },
  callback = function()
    vim.bo.filetype = 'yaml'
  end,
})
