vim.api.nvim_create_autocmd({"BufNewFile", "BufRead"}, {
  pattern = "term://*zsh",
  callback = function()
    vim.bo.filetype = "term-shell"
  end,
})