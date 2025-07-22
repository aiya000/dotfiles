vim.opt_local.tabstop = 2
vim.opt_local.shiftwidth = 2
vim.opt_local.list = false
vim.opt_local.nu = false
vim.opt_local.rnu = false
vim.opt_local.wrap = false

vim.keymap.set(
  'n',
  '<buffer><silent>',
  '<C-r> :<C-u>call <SID>reopen_stack_build()<CR>',
  { buffer = true, silent = true }
)

vim.cmd([[
function! s:reopen_stack_build() abort
  let bufnr = winbufnr('.')
  call vimrc#open_terminal_as('stack_test', 'horizontal', 'stack build', {'path': g:vimrc.path_at_started, 'noclose': v:true})
  execute 'bdelete' bufnr
endfunction
]])
