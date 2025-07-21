vim.api.nvim_create_autocmd({"BufNewFile", "BufRead"}, {
  pattern = {".vimperatorrc", ".vrapperrc", ".vsvimrc", "vimium-c-keymaps"},
  callback = function()
    vim.bo.filetype = "vim"
  end,
})