-- TODO: Load haskell ftplugin

vim.opt_local.commentstring = '{- %s -}'
vim.opt_local.ts = 2
vim.opt_local.sw = 2
vim.opt_local.et = true
vim.opt_local.tw = 0

local function align()
  vim.cmd("'<,'>Alignta => {\\(\\s\\|%\\)")
  vim.cmd("'<,'>Alignta => \\s}")
end

vim.keymap.set('v', 'i{', align, { buffer = true, silent = true })
