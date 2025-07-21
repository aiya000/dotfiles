vim.api.nvim_create_autocmd({"BufNewFile", "BufRead"}, {
  pattern = "*.re",
  callback = function()
    vim.bo.filetype = "review"
  end,
})