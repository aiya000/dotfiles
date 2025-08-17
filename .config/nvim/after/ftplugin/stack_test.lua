vim.cmd("execute 'source' (InitLua.neovim_home . '/after/ftplugin/stack_build.vim')")

vim.keymap.set(
  'n',
  '<buffer><silent>',
  '<C-r> :<C-u>call <SID>reopen_stack_test()<CR>',
  { buffer = true, silent = true }
)

vim.cmd([[
function s:reopen_stack_test() abort
  const bufnr = winbufnr('.')

  call vimrc#open_terminal_as('stack_test', 'horizontal', 'stack test --fast', {
    \ 'path': g:vimrc.path_at_started,
    \ 'noclose': v:true,
  \ })

  execute 'bdelete' bufnr
endfunction
]])
