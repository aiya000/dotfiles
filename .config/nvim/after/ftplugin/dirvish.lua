-- NOTE: ? Prevent appearing the character '«' at the beginning of a line when opened
vim.cmd('normal! 0')

vim.cmd('nmap <buffer> H <Plug>(dirvish_up)')
vim.cmd('nmap <buffer> E eb')
vim.keymap.set('n', '/', '/', { buffer = true })
vim.keymap.set('n', '<buffer>', '<C-n> gT', { buffer = true })
vim.keymap.set('n', '<buffer>', '<C-n> gt', { buffer = true })
vim.keymap.set('n', '?', '?', { buffer = true })
vim.keymap.set('n', 'Q', function()
  vim.cmd('quit')
end, { buffer = true })
vim.keymap.set('n', 'd', ':!mkdir<Space>', { buffer = true })
vim.keymap.set('n', 'D', function()
  vim.cmd('call <SID>delete_file_or_dir()')
end, { buffer = true, silent = true })

-- Redraw
vim.keymap.set('n', '<buffer>', '<C-r> :Dirvish<CR>', { buffer = true })

vim.cmd([[
function s:delete_file_or_dir() abort
  const file_or_dir_name = getline('.')
  if confirm($"Deleting '{file_or_dir_name}' ?", "&Yes\n&No") isnot 1
    echomsg 'Canceled'
    return
  endif

  if file_or_dir_name =~# '/$'  " if it is a directory
    call system($'rmdir ' .. file_or_dir_name)
    echomsg $'Deleted: {file_or_dir_name}'
    return
  endif
  " if it is a file
  call system('rm ' .. file_or_dir_name)
  echomsg $'Deleted: {file_or_dir_name}'
endfunction
]])
