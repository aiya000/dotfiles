vim.api.nvim_create_autocmd({"BufNewFile", "BufRead"}, {
  pattern = "*.x",
  callback = function()
    vim.bo.filetype = "alex"
  end,
})