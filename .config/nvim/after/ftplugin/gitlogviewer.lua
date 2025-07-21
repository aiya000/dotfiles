-- This filetype was presented by plugin/gitlogviewer.vim

vim.cmd("let s:V = vital#vimrc#new()")
vim.cmd("let s:List = s:V.import('Data.List')")


vim.opt_local.list = false
vim.opt_local.cul = true

vim.cmd([[
function s:try_show_git_show() abort
  try
    call s:show_git_show()
  catch
    echomsg v:exception
    close
    call s:show_git_show()
  endtry
endfunction
]])

vim.cmd([[
function s:show_git_show() abort
  normal! [z
  vsplit
  if s:List.has(split(b:gitlogviewer_args), '--oneline')
    normal! _"zyiw
  else
    " TODO: Currently, this is not working if I'm on wrapped line.
    normal! g_"zyiw
  endif

  execute 'GitShowViewer' @z
endfunction
]])

vim.keymap.set('n', "Q", function() vim.cmd("<C-u>bdelete!") end, { buffer = true, silent = true })
vim.keymap.set('n', "S", function() vim.call("<SID>try_show_git_show()") end, { buffer = true, silent = true })
vim.keymap.set('n', "p", function() vim.call("<SID>try_show_git_show()") end, { buffer = true, silent = true })
vim.keymap.set('n', "<buffer><silent>", "<C-r> :<C-u>GitLogViewer <C-r>=b:gitlogviewer_args<CR><CR>", { buffer = true, silent = true })
