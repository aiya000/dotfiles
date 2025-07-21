vim.api.nvim_create_autocmd({"BufNewFile", "BufRead"}, {
  pattern = "*.hamlet",
  callback = function()
    vim.bo.filetype = "hamlet"
  end,
})