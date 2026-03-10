vim.api.nvim_create_autocmd({ 'BufNewFile', 'BufRead' }, {
  pattern = '.gitconfig_env',
  callback = function()
    vim.bo.filetype = 'gitconfig'
  end,
})
