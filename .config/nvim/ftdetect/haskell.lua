vim.api.nvim_create_autocmd({"BufNewFile", "BufRead"}, {
  pattern = {".xmobarrc", "*.hsc"},
  callback = function()
    vim.bo.filetype = "haskell"
  end,
})