vim.api.nvim_create_autocmd({"BufNewFile", "BufRead"}, {
  pattern = "*.ps1",
  callback = function()
    vim.bo.filetype = "ps1"
  end,
})