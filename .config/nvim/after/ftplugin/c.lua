
vim.opt.commentstring = "  // %s"

vim.keymap.set('n', "r", function() vim.call("<SID>run_quickfix()") end, { buffer = true, silent = true })
vim.keymap.set('n', "w", function() vim.call("<SID>start_quickfix()") end, { buffer = true, silent = true })
vim.keymap.set('n', "W", function() vim.call("<SID>stop_quickfix()") end, { buffer = true, silent = true })

local augroup_FtpluginC = vim.api.nvim_create_augroup("FtpluginC", { clear = true })
  vim.api.nvim_create_autocmd("BufWritePost", { group = augroup_FtpluginC, pattern = "*.c,*.h", callback = function() vim.cmd("call s:exec_quickfix_if_available()") end })

vim.cmd([[
function! s:run_quickfix() abort
  QuickfixRunMake  -j4  -e  CFLAGS='-g3 -O0'  -Wall
endfunction
]])

vim.cmd([[
function! s:start_quickfix() abort
  let s:does_quickfix_watch = v:true
  QuickfixRunMake  -j4  -e  CFLAGS='-g3 -O0'  -Wall
endfunction
]])

vim.cmd([[
function! s:stop_quickfix() abort
  let s:does_quickfix_watch = v:false
  cclose
endfunction
]])

vim.cmd([[
function! s:exec_quickfix_if_available() abort
  if get(s:, 'does_quickfix_watch', v:false)
    QuickfixRunMake  -j4  -e  CFLAGS='-g3 -O0'  -Wall
  endif
endfunction
]])
