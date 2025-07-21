-- eliningen
vim.api.nvim_create_autocmd({"BufNewFile", "BufRead"}, {
  pattern = "*.elin",
  callback = function()
    vim.bo.filetype = "lisp"
  end,
})