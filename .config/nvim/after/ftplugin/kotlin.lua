
vim.opt_local.ts = 4
vim.opt_local.sw = 4
vim.opt_local.et = true
vim.opt.errorformat = "%t: %f: (%l\, %c): %m"

vim.keymap.set('n', "r", function() vim.cmd("<C-u>QuickfixRunGradle build") end, { buffer = true, silent = true })
vim.keymap.set('n', "R", function() vim.call("vimrc#open_terminal_as('term-gradle', 'horizontal', "bash -c 'cd $(git rev-parse --show-toplevel) && gradle run'", v:false)") end, { buffer = true, silent = true })
vim.keymap.set('n', "w", function() vim.call("<SID>start_quickfix()") end, { buffer = true, silent = true })
vim.keymap.set('n', "W", function() vim.call("<SID>stop_quickfix()") end, { buffer = true, silent = true })
vim.keymap.set('n', ":syntax", "sync fromstart<CR>", { buffer = true, silent = true })
vim.cmd("nmap <buffer> <C-l> <Esc>")

vim.cmd("syntax sync fromstart")

local augroup_FtpluginKotlin = vim.api.nvim_create_augroup("FtpluginKotlin", { clear = true })
  vim.api.nvim_create_autocmd("BufWritePost", { group = augroup_FtpluginKotlin, pattern = "*.kt", callback = function() vim.cmd("call s:exec_quickfix_if_available()") end })

vim.cmd([[
function! s:start_quickfix() abort
  let s:does_quickfix_watch = v:true
  QuickfixRunGradle build
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
    QuickfixRunGradle build
  endif
endfunction
]])
