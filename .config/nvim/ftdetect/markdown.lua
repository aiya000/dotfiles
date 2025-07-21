vim.api.nvim_create_autocmd({"BufNewFile", "BufRead"}, {
  pattern = {"*.md", "*.mdx", "*.mdpp", ".clinerules"},
  callback = function()
    vim.bo.filetype = "markdown"
  end,
})