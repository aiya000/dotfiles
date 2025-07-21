vim.api.nvim_create_autocmd({"BufNewFile", "BufRead"}, {
  pattern = "*.v",
  callback = function()
    vim.bo.filetype = "coq"
  end,
})