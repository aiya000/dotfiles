vim.api.nvim_create_autocmd({ 'BufNewFile', 'BufRead' }, {
  pattern = '*.rules',
  callback = function()
    vim.bo.filetype = 'firestore'
  end,
})
