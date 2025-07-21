vim.api.nvim_create_autocmd({"BufNewFile", "BufRead"}, {
  pattern = ".ghci",
  callback = function()
    vim.bo.filetype = "dot-ghci"
  end,
})