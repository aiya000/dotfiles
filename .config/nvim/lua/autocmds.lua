---NOTE:
---特定のプラグインに関連する操作をここに書かないでください。
---我々はこれによって、繰り返し失敗してきました

local ToggleBuftype = require('models.ToggleBuftype')
local toggle_buftype = ToggleBuftype.new()
local s = require('utils.functions').s
local helper = require('helper')

local M = {}

M.AugroupInitLua = vim.api.nvim_create_augroup('InitLua', { clear = true })

---@param event string | string[]
---@param callback (fun(): nil) | string --Lua function or Vim script function name
---@param pattern? string | string[] --Pattern for FileType events
---@see 'installed-dir/share/nvim/runtime/doc/api.txt'
function M.add_autocmd(event, callback, pattern)
  local opts = {
    group = AugroupInitLua,
    callback = callback,
  }
  if pattern ~= nil then
    opts.pattern = pattern
  end
  vim.api.nvim_create_autocmd(event, opts)
end

-- Alias for another file
-- Example:
-- ```lua
-- local autocmds = require('autocmds')
--
-- autocmds.add('FooEvent', function()
--   -- do something
-- end)
-- ```
M.add = M.add_autocmd

M.add_autocmd('VimEnter', function()
  vim.cmd('ScdCurrentDir')
end)

-- Move the cursor position to the last position of a file
M.add_autocmd('BufReadPost', function()
  vim.cmd(':' .. vim.fn.line('\'"'))
end)

-- Setup Cmdwin
M.add_autocmd('CmdwinEnter', function()
  vim.keymap.set('n', 'Q', '<Cmd>q<CR>', { buffer = true, silent = true })
  vim.keymap.set('n', 'ghq', '<Cmd>q<CR>', { buffer = true, silent = true })
  vim.keymap.set('n', 'ghQ', '<Cmd>qall<CR>', { buffer = true, silent = true })
  vim.keymap.set('n', '<C-j>', '<C-c><CR>', { buffer = true })
  vim.keymap.set('n', '<CR>', '<C-c><CR>', { buffer = true })

  if vim.fn.getcmdwintype() == ':' then
    vim.keymap.set('i', '<C-n>', function()
      return vim.fn.pumvisible() == 1 and '<C-n>' or '<C-x><C-v>'
    end, { buffer = true, expr = true })
    vim.keymap.set('i', '<C-p>', function()
      return vim.fn.pumvisible() == 1 and '<C-p>' or '<C-x><C-v><C-p><C-p>'
    end, { buffer = true, expr = true })
  end
end)

-- Show :help by vertical split default
M.add_autocmd('BufWinEnter', function()
  if vim.bo.filetype == 'help' then
    vim.cmd('wincmd H')
  end
end, { '*.txt' })

-- `buftype=nofile`でddcを動かすと何かが起こったので（なんだっけ？）、一時的に`buftype=`にする
M.add_autocmd('InsertEnter', function()
  toggle_buftype:backup_buftype()
end)

M.add_autocmd('InsertLeave', function()
  toggle_buftype:restore_buftype()
end)

-- TODO: Neovimでこれ動いてる？
-- Show simply for terminal buffers
M.add_autocmd('TermOpen', function()
  vim.opt_local.list = false
  vim.opt_local.number = false
  vim.opt_local.relativenumber = false
end)

-- Show relative numbers only on the current window
M.add_autocmd({ 'BufEnter', 'WinEnter' }, function()
  if vim.wo.number then
    vim.wo.relativenumber = true
  end
end)

M.add_autocmd({ 'BufLeave', 'WinLeave' }, function()
  vim.wo.relativenumber = false
end)

-- Show full-width spaces
M.add_autocmd('ColorScheme', function()
  vim.api.nvim_set_hl(0, 'EmSpace', { ctermbg = 'LightBlue', bg = 'LightBlue' })
end)

M.add_autocmd({ 'VimEnter', 'WinEnter' }, function()
  vim.fn.matchadd('EmSpace', '　')
end)

-- Colorize git conflicts
M.add_autocmd('ColorScheme', function()
  vim.api.nvim_set_hl(0, 'GitConflict', { ctermbg = 'Red', bg = 'Red' })
end)

M.add_autocmd({ 'VimEnter', 'WinEnter' }, function()
  vim.fn.matchadd('GitConflict', [[^\(<\|=\|>\)\{7\}\([^=].\+\)\?$]])
end)

-- StatusLine
M.add_autocmd('InsertEnter', function()
  vim.api.nvim_set_hl(0, 'StatusLine', { ctermfg = 231, ctermbg = 64 })
end)

M.add_autocmd('InsertLeave', function()
  vim.api.nvim_set_hl(0, 'StatusLine', { ctermfg = 231, ctermbg = 60 })
end)

-- Hightlight cursors of another windows
M.add_autocmd({ 'BufEnter', 'WinEnter' }, function()
  vim.opt_local.cursorline = false
end)

M.add_autocmd({ 'BufLeave', 'WinLeave' }, function()
  vim.opt_local.cursorline = true
end)

M.add_autocmd({ 'VimEnter', 'ColorScheme' }, function()
  vim.api.nvim_set_hl(0, 'CursorLine', { ctermbg = 60 })
end)

-- Language specific autocmds
local natural_language_filetypes = {
  '',
  'markdown',
  'txt',
}

-- TODO: ここでエラーが出るけど、後でcoc.nvimに置き換えるので、今はコメントアウト
-- > Error detected while processing BufEnter Autocommands for "*":
-- > Error executing lua callback: Vim:E117: Unknown function: ddc#disable
-- > stack traceback:
-- >         [C]: in function 'ddc#disable'
-- >         /home/aiya000/.config/nvim/lua/autocmds.lua:142: in function </home/aiya000/.config/nvim/lua/autocmds.lua:140>
-- -- 自然言語を書いているとddcが重すぎるので、一時的に無効化する
-- M.add_autocmd({ 'BufEnter', 'WinEnter' }, function()
--   if vim.tbl_contains(natural_language_filetypes, vim.bo.filetype) then
--     vim.fn['ddc#disable']()
--   else
--     vim.fn['ddc#enable']()
--   end
-- end)

-- Scala configuration
M.add_autocmd('VimEnter', function()
  if vim.fn.filereadable('./scalastyle_config.xml') == 1 then
    local answer = vim.fn.input('locally scalastyle_config.xml was found, Do you want to load? (y/n)')
    if answer == 'y' then
      local cwd = vim.fn.getcwd()
      vim.g.ale_scala_scalastyle_config = s('{cwd}/scalastyle-config.xml', { cwd = cwd })
      vim.cmd(s('echomsg "a scalastyle config loaded: {config}"', { config = vim.g.ale_scala_scalastyle_config }))
    end
  end
end)

-- vim-fmap {{{

M.add_autocmd('VimEnter', function()
  vim.cmd('FNoreMap / ・')
  vim.cmd('FNoreMap T ・')
  vim.cmd('FNoreMap tt …')
  vim.cmd("FNoreMap '' 　")
  vim.cmd('FNoreMap p （')
  vim.cmd('FNoreMap k 「')
  vim.cmd('FNoreMap K 〈')
  vim.cmd('FNoreMap -k 『')
end)

-- }}}
-- vim-precious {{{

M.add_autocmd({ 'WinEnter', 'BufEnter', 'TabEnter' }, function()
  pcall(vim.cmd, 'PreciousSwitch') -- もしvim-preciousがまだ読み込まれていなかったら、何もしない
end)

-- }}}
-- vim-cursorword {{{

M.add_autocmd({ 'VimEnter', 'ColorScheme' }, function()
  vim.api.nvim_set_hl(0, 'CursorWord0', { ctermbg = 'LightGray', ctermfg = 'Black' })
  vim.api.nvim_set_hl(0, 'CursorWord1', { ctermbg = 'LightGray', ctermfg = 'Black' })
end)

-- }}}
-- AsyncRun {{{

M.add_autocmd('User', function()
  vim.call('vimrc#popup_atcursor', ':AsyncRun finished')
end, 'AsyncRunStop')

-- }}}

-- }}}

return M
