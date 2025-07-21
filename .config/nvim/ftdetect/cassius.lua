vim.api.nvim_create_autocmd({"BufNewFile", "BufRead"}, {
  pattern = "*.cassius",
  callback = function()
    vim.bo.filetype = "cassius"
  end,
})