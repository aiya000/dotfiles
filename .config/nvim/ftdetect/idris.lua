vim.api.nvim_create_autocmd({"BufNewFile", "BufRead"}, {
  pattern = {"*.idr", "idris-response"},
  callback = function()
    vim.bo.filetype = "idris"
  end,
})