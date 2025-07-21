vim.api.nvim_create_autocmd({"BufNewFile", "BufRead"}, {
  pattern = "*.shader",
  callback = function()
    vim.bo.filetype = "shaderlab"
  end,
})