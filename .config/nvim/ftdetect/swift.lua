vim.api.nvim_create_autocmd({"BufNewFile", "BufRead"}, {
  pattern = "*.swift",
  callback = function()
    vim.bo.filetype = "swift"
  end,
})