---特定のプラグインに関連する操作をここに書かないでください。
---我々はこれによって、繰り返し失敗してきました

local helper = require('helper')

local M = {}

local augroup = vim.api.nvim_create_augroup('InitLuaAutocmds', { clear = true })

-- Move the cursor position to the last position of a file
vim.api.nvim_create_autocmd('BufReadPost', {
  group = augroup,
  callback = function()
    vim.cmd(':' .. vim.fn.line('\'"'))
  end,
})

-- Setup Cmdwin
vim.api.nvim_create_autocmd('CmdwinEnter', {
  group = augroup,
  callback = function()
    vim.keymap.set('n', 'Q', '<Cmd>quit<CR>', { buffer = true, silent = true })
    vim.keymap.set('n', 'ghq', '<Cmd>quit<CR>', { buffer = true, silent = true })
    vim.keymap.set('n', '<C-l>', '<Cmd>quit<CR>', { buffer = true })
    vim.keymap.set('n', 'ghQ', '<Cmd>qall<CR>', { buffer = true, silent = true })
    vim.keymap.set('n', '<C-j>', '<Esc><CR>', { buffer = true })
    vim.keymap.set('n', '<CR>', '<Esc><CR>', { buffer = true })
  end,
})

-- Show :help by vertical split default
vim.api.nvim_create_autocmd('BufWinEnter', {
  group = augroup,
  pattern = { '*.txt' },
  callback = function()
    if vim.bo.filetype == 'help' then
      vim.cmd('wincmd H')
    end
  end,
})

-- cmdpalette.nvimと連携するための、疑似的な`nnoremap ::: :<C-u>%s/`
vim.api.nvim_create_autocmd('CmdlineChanged', {
  group = augroup,
  callback = function()
    helper.replace_line(
      {
        [':'] = '%s/',
      },
      vim.fn.getcmdline(),
      vim.fn.setcmdline
    )
  end,
})

-- Highlighting and ColorSchema {{{

vim.api.nvim_create_autocmd({'VimEnter', 'ColorScheme'}, {
  group = augroup,
  callback = function()
    vim.api.nvim_set_hl(0, 'CursorLine', { bg = '#313244' })
    vim.api.nvim_set_hl(0, 'TrailingSpace', { ctermbg = 'Red', bg = '#F38BA8' })

    vim.api.nvim_set_hl(0, 'EmSpace', { ctermbg = 'LightBlue', bg = '#89B4FA' })
    vim.fn.matchadd('EmSpace', '　')

    -- TODO: *.luaで、'TODO:', 'FIXME:', 'NOTE:' のハイライトがいつの間にか消える
    vim.api.nvim_set_hl(0, 'HighlightFixme', { ctermbg = 'Red', ctermfg = 'White', bg = '#EBA0AC', fg = '#1E1E2E', bold = true })
    vim.fn.matchadd('HighlightFixme', [[\<FIXME\>:]])

    vim.api.nvim_set_hl(0, 'HighlightTodo', { ctermbg = 'Yellow', ctermfg = 'Black', bg = '#F9E2AF', fg = '#1E1E2E', bold = true })
    vim.fn.matchadd('HighlightTodo', [[\<TODO\>:]])

    vim.api.nvim_set_hl(0, 'HighlightNote', { ctermbg = 'Cyan', ctermfg = 'Black', bg = '#94E2D5', fg = '#1E1E2E', bold = true })
    vim.fn.matchadd('HighlightNote', [[\<NOTE\>:]])
  end,
})

-- Highlight for FileTypes
vim.api.nvim_create_autocmd('FileType', {
  group = augroup,
  callback = function()
    local excluded_filetypes = { 'terminal-shell', 'toggleterm', 'git-log', 'git-show', 'gin-log' }
    if vim.tbl_contains(excluded_filetypes, vim.bo.filetype) then
      return
    end
    vim.fn.matchadd('TrailingSpace', [[\s\+$]])
  end,
})

-- }}}
-- Show CursorLine only on the current window {{{

vim.api.nvim_create_autocmd({ 'BufEnter', 'WinEnter' }, {
  group = augroup,
  callback = function()
    vim.opt.cursorline = true
  end,
})

vim.api.nvim_create_autocmd({ 'BufLeave', 'WinLeave' }, {
  group = augroup,
  callback = function()
    vim.opt.cursorline = false
  end,
})

-- }}}
-- Show relative numbers only on the current window {{{

vim.api.nvim_create_autocmd({ 'BufEnter', 'WinEnter' }, {
  group = augroup,
  callback = function()
    if vim.wo.number then
      vim.wo.relativenumber = true
    end
  end,
})

vim.api.nvim_create_autocmd({ 'BufLeave', 'WinLeave' }, {
  group = augroup,
  callback = function()
    vim.wo.relativenumber = false
  end,
})

-- }}}

return M
