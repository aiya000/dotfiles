vim.api.nvim_create_autocmd({"BufNewFile", "BufRead"}, {
  pattern = {".babelrc", ".csslintrc", ".firebaserc", ".htmlhintrc", ".htmllintrc", ".jshintrc"},
  callback = function()
    vim.bo.filetype = "json"
  end,
})