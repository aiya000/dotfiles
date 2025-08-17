vim.cmd("execute 'source' (InitLua.neovim_home . '/after/ftplugin/haskell.vim')")

vim.opt.commentstring = '{- %s -}'
vim.opt_local.ts = 2
vim.opt_local.sw = 2
vim.opt_local.et = true
vim.opt_local.tw = 0

vim.keymap.set('v', 'i{', function()
  vim.call('<SID>align()')
end, { buffer = true, silent = true })

vim.cmd([[
function! s:align() abort
  '<,'>Alignta => {\(\s\|%\)
  '<,'>Alignta => \s}
endfunction
]])
