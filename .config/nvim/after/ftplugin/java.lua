vim.opt.commentstring = " /*%s*/"
vim.opt_local.ts = 2
vim.opt_local.sw = 2
vim.opt_local.tw = 100

vim.keymap.set('n', "r", function() vim.cmd("<C-u>QuickfixRunGradle build") end, { buffer = true, silent = true })
vim.keymap.set('n', "R", function() vim.call("vimrc#open_terminal_as('term-gradle', 'horizontal', "gradle run'", {'path': g:vimrc.path_at_started, 'noclose': v:true})") end, { buffer = true, silent = true })
vim.keymap.set('n', "w", function() vim.call("<SID>start_quickfix()") end, { buffer = true, silent = true })
vim.keymap.set('n', "W", function() vim.call("<SID>stop_quickfix()") end, { buffer = true, silent = true })
vim.keymap.set('n', "c", function() vim.call("vimrc#open_terminal_as('term-gradle', 'horizontal', "gradle clean'", {'path': g:vimrc.path_at_started, 'noclose': v:true})") end, { buffer = true, silent = true })
vim.keymap.set('n', ":syntax", "sync fromstart<CR>", { buffer = true, silent = true })
vim.cmd("nmap <buffer> <C-l> <Esc>")

vim.cmd("syntax sync fromstart")

local augroup_FtpluginJava = vim.api.nvim_create_augroup("FtpluginJava", { clear = true })
  vim.api.nvim_create_autocmd("BufWritePost", { group = augroup_FtpluginJava, pattern = "*.java", callback = function() vim.cmd("call s:exec_quickfix_if_available()") end })

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
