vim.api.nvim_create_autocmd({"BufNewFile", "BufRead"}, {
  pattern = "*.fxml",
  callback = function()
    vim.bo.filetype = "fxml"
  end,
})