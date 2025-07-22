vim.opt_local.number = false
vim.opt_local.relativenumber = false
vim.opt_local.list = false

vim.keymap.set('n', 'r', 'i<End><C-u>:reload<CR>', { buffer = true })
vim.keymap.set('n', 's', function()
  vim.call('<SID>open_say_buffer()<CR>i')
end, { buffer = true })
vim.keymap.set('n', 'S', 'i:show breaks<CR>', { buffer = true, silent = true })
vim.keymap.set('n', 'o', 'i:step<CR>:list<CR>', { buffer = true, silent = true })
vim.keymap.set('n', 'O', 'i:list<CR>', { buffer = true, silent = true })
vim.keymap.set('n', 'A', 'i:steplocal<CR>:list<CR>', { buffer = true, silent = true })
vim.keymap.set('n', 'S', function()
  vim.cmd("<C-u>Stackage <C-r>=expand('<cword>')<CR>")
end, { buffer = true })

vim.cmd([[
function! s:open_say_buffer() abort
  let s:ghci_bufnr = winbufnr('.')
  botright new
  call s:set_default_buffer_prefs()
  call s:define_default_keymaps()
  normal! i
endfunction
]])

vim.cmd([[
function! s:set_default_buffer_prefs() abort
  setl filetype=haskell buftype=nofile noreadonly modifiable
  setl tabstop=2 shiftwidth=2 expandtab
  setl syntax=haskell
  let b:ale_enabled = v:false
  resize 5
endfunction
]])

vim.cmd([[
function! s:define_default_keymaps() abort
  nnoremap <buffer> <C-m> :<C-u>call <SID>say()<CR>
endfunction
]])

vim.cmd([[
function! s:say() abort
  let r = @"
  let z = @z
  normal! gg"zyG
  let [detail, @z, @"] = [@z, z, r]
  let detail = ":{\<CR>" . detail . "\<CR>:}\<CR>"

  call term_sendkeys(s:ghci_bufnr, detail)
  quit
endfunction
]])
