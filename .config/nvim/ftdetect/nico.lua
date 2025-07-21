vim.api.nvim_create_autocmd({"BufNewFile", "BufRead"}, {
  pattern = "*.nico",
  callback = function()
    vim.bo.filetype = "nico"
  end,
})