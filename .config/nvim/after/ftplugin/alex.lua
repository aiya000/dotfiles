vim.cmd("execute 'source' (g:vimrc['vim_home'] . '/after/ftplugin/haskell.vim')")


vim.opt.commentstring = "-- %s"
vim.opt_local.ts = 2
vim.opt_local.sw = 2
vim.opt_local.et = true
vim.opt_local.tw = 0

vim.keymap.set('v', "i{", function() vim.cmd("Alignta => {<CR>gv:Alignta => }") end, { buffer = true, silent = true })
