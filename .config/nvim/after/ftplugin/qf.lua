vim.opt_local.number = false
vim.opt_local.relativenumber = false
vim.opt_local.cursorline = true
vim.opt_local.list = false
vim.opt_local.wrap = true

local function modulo(a, b)
  return a - math.floor(a / b) * b
end

local function go_to_errorformat(motion)
  local max = vim.fn.line('$')
  local list = vim.fn.getloclist(0)
  if vim.tbl_isempty(list) or #list ~= max then
    list = vim.fn.getqflist()
  end
  local cur = vim.fn.line('.') - 1
  local pos = modulo(cur + motion, max)
  local m = motion > 0 and 1 or -1

  while cur ~= pos and list[pos + 1].bufnr == 0 do
    pos = modulo(pos + m, max)
  end

  return (pos + 1) .. 'G'
end

local function open_vertical_new()
  vim.cmd('normal! g_hgf')
  vim.cmd('vsp')
  vim.cmd('wincmd w')
  vim.cmd('normal! ghH')
  vim.cmd('normal! <C-\\><C-o>')
end

vim.keymap.set('n', 'Q', function()
  vim.cmd('bdelete')
end, { buffer = true })

vim.keymap.set('n', 'p', open_vertical_new, { buffer = true })
vim.keymap.set('n', '<C-j>', '<CR>', { buffer = true })

vim.keymap.set('n', 'cc', function()
  vim.fn.setqflist({})
end, { buffer = true })

vim.keymap.set('n', '<C-a>', function()
  local count = vim.v.count1
  local cmd = go_to_errorformat(count)
  vim.cmd('normal! ' .. cmd)
end, { buffer = true, silent = true, nowait = true })

vim.keymap.set('n', '<C-x>', function()
  local count = vim.v.count1
  local cmd = go_to_errorformat(-count)
  vim.cmd('normal! ' .. cmd)
end, { buffer = true, silent = true, nowait = true })
