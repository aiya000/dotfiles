vim.api.nvim_create_autocmd({"BufNewFile", "BufRead"}, {
  pattern = "*.vue",
  callback = function()
    vim.bo.filetype = "vue"
  end,
})