vim.cmd("let s:Math = vital#vimrc#import('Math')")


vim.opt_local.number = false
vim.opt_local.relativenumber = false
vim.opt_local.cursorline = true
vim.opt_local.list = false
vim.opt_local.wrap = true

vim.keymap.set('n', "Q", function() vim.cmd("bdelete") end, { buffer = true })
vim.keymap.set('n', "p", function() vim.cmd("call <SID>open_vertical()") end, { buffer = true })
vim.keymap.set('n', "<buffer>", "<C-j> <CR>", { buffer = true })
vim.keymap.set('n', "call", "setqflist([])<CR>", { buffer = true })
vim.keymap.set('n', "<buffer><silent>", "<expr><nowait> <C-a> <SID>go_to_errorformat(v:count1)", { buffer = true, silent = true })
vim.keymap.set('n', "<buffer><silent>", "<expr><nowait> <C-x> <SID>go_to_errorformat(-v:count1)", { buffer = true, silent = true })

-- Thanks thinca
vim.cmd([[
function! s:go_to_errorformat(motion)
  let max = line('$')
  let list = getloclist(0)
  if empty(list) || len(list) != max
    let list = getqflist()
  endif
  let cur = line('.') - 1
  let pos = s:Math.modulo(cur + a:motion, max)
  let m = 0 < a:motion ? 1 : -1
  while cur != pos && list[pos].bufnr == 0
    let pos = s:Math.modulo(pos + m, max)
  endwhile
  return (pos + 1) . 'G'
endfunction
]])

vim.cmd([[
function! s:open_vertical() abort
  normal! g_hgf
  vsp
  execute 'normal!' "\<C-w>w"
  " TODO: vimrcでnmapしたものに依存しているので、`normal!`でできるようにする
  normal ghH
  execute 'normal!' "\<C-o>"
endfunction
]])
