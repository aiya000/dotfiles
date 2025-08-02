---NOTE:
---特定のプラグインに関連する操作をここに書かないでください。
---我々はこれによって、繰り返し失敗してきました

local ToggleBuftype = require('models.ToggleBuftype')
local toggle_buftype = ToggleBuftype.new()
local s = require('utils.functions').s

---@param event string | string[]
---@param callback (fun(): nil) | string --Lua function or Vim script function name
---@param pattern? string | string[] --Pattern for FileType events
---@see 'installed-dir/share/nvim/runtime/doc/api.txt'
local function add_autocmd(event, callback, pattern)
  local default_group = vim.api.nvim_create_augroup('InitLua', { clear = true })
  local opts = {
    group = default_group,
    callback = callback,
  }
  if pattern then
    opts.pattern = pattern
  end
  vim.api.nvim_create_autocmd(event, opts)
end

---Avoids nmap onto unterminal buffer
local function nmap_p_to_put_if_on_terminal()
  print('TODO: Not Implemented Yet (nmap_p_to_put_if_on_terminal)')
  -- TODO: Implement term_list() equivalent for Neovim
  -- if not vim.tbl_contains(vim.fn.term_list(), vim.fn.winbufnr('.')) then
  --   return
  -- end
  -- TODO: Implement put_as_stdin equivalent
  -- vim.keymap.set('n', 'p', 'vimrc#put_as_stdin(@")', { buffer = true, expr = true })
end

---Simular to nmap_p_to_put_if_on_terminal()
local function nmap_plus_p_to_put_if_on_terminal()
  print('TODO: Not Implemented Yet (nmap_plus_p_to_put_if_on_terminal)')
  -- TODO: Implement term_list() equivalent for Neovim
  -- if not vim.tbl_contains(vim.fn.term_list(), vim.fn.winbufnr('.')) then
  --   return
  -- end
  -- TODO: Implement put_as_stdin equivalent
  -- vim.keymap.set('n', '"+p', 'vimrc#put_as_stdin(@+)', { buffer = true, expr = true })
end

local function setup_cmdwin()
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
end

add_autocmd('VimEnter', function()
  vim.cmd('ScdCurrentDir')
end)

-- TODO: Implement visit_past_position equivalent
-- add_autocmd('BufReadPost', function()
--   vim.call('vimrc#visit_past_position')
-- end)

add_autocmd('CmdwinEnter', setup_cmdwin)

-- `buftype=nofile`でddcを動かすと何かが起こったので（なんだっけ？）、一時的に`buftype=`にする
add_autocmd('InsertEnter', function()
  toggle_buftype:backup_buftype()
end)

add_autocmd('InsertLeave', function()
  toggle_buftype:restore_buftype()
end)

-- NOTE: ↓このコメントどういう意味？
-- TODO: for any registers
-- TODO: Implement terminal mappings for non-nvim

-- Show simply for terminal buffers
add_autocmd('TermOpen', function()
  vim.opt_local.list = false
  vim.opt_local.number = false
  vim.opt_local.relativenumber = false
end)

-- Show relative numbers only on the current window
add_autocmd({ 'BufEnter', 'WinEnter' }, function()
  if vim.wo.number then
    vim.wo.relativenumber = true
  end
end)

add_autocmd({ 'BufLeave', 'WinLeave' }, function()
  vim.wo.relativenumber = false
end)

-- Show full-width spaces
add_autocmd('ColorScheme', function()
  vim.api.nvim_set_hl(0, 'EmSpace', { ctermbg = 'LightBlue', bg = 'LightBlue' })
end)

add_autocmd({ 'VimEnter', 'WinEnter' }, function()
  vim.fn.matchadd('EmSpace', '　')
end)

-- Colorize git conflicts
add_autocmd('ColorScheme', function()
  vim.api.nvim_set_hl(0, 'GitConflict', { ctermbg = 'Red', bg = 'Red' })
end)

add_autocmd({ 'VimEnter', 'WinEnter' }, function()
  vim.fn.matchadd('GitConflict', [[^\(<\|=\|>\)\{7\}\([^=].\+\)\?$]])
end)

-- StatusLine
add_autocmd('InsertEnter', function()
  vim.api.nvim_set_hl(0, 'StatusLine', { ctermfg = 231, ctermbg = 64 })
end)

add_autocmd('InsertLeave', function()
  vim.api.nvim_set_hl(0, 'StatusLine', { ctermfg = 231, ctermbg = 60 })
end)

-- Hightlight cursors of another windows
add_autocmd({ 'BufEnter', 'WinEnter' }, function()
  vim.opt_local.cursorline = false
end)

add_autocmd({ 'BufLeave', 'WinLeave' }, function()
  vim.opt_local.cursorline = true
end)

add_autocmd({ 'VimEnter', 'ColorScheme' }, function()
  vim.api.nvim_set_hl(0, 'CursorLine', { ctermbg = 60 })
end)

-- Language specific autocmds
local natural_language_filetypes = {
  '',
  'markdown',
  'txt',
}

-- 自然言語を書いているとddcが重すぎるので、一時的に無効化する
add_autocmd({ 'BufEnter', 'WinEnter' }, function()
  if vim.tbl_contains(natural_language_filetypes, vim.bo.filetype) then
    -- TODO: Implement ddc#disable() equivalent
    -- vim.call('ddc#disable')
  else
    -- TODO: Implement ddc#enable() equivalent
    -- vim.call('ddc#enable')
  end
end)

-- TypeScript/JavaScript specific
local function read_deno_local_tsconfig()
  local cwd = vim.fn.getcwd()
  local local_tsconfig = s'{cwd}/tsconfig.json'
  if vim.fn.filereadable(local_tsconfig) == 1 then
    vim.g.ale_javascript_deno_lint_options = s'--config {local_tsconfig}'
  end
end

add_autocmd('FileType', read_deno_local_tsconfig, { 'typescript', 'javascript' })

-- ALE color scheme
add_autocmd('ColorScheme', function()
  vim.api.nvim_set_hl(0, 'ALEError', { ctermbg = 'gray', ctermfg = 'black' })
end)

-- Scala configuration
add_autocmd('VimEnter', function()
  if vim.fn.filereadable('./scalastyle_config.xml') == 1 then
    local answer = vim.fn.input('locally scalastyle_config.xml was found, Do you want to load? (y/n)')
    if answer == 'y' then
      vim.g.ale_scala_scalastyle_config = s'{vim.fn.getcwd()}/scalastyle-config.xml'
      vim.cmd(s'echomsg "a scalastyle config loaded: {vim.g.ale_scala_scalastyle_config}"')
    end
  end
end)

-- vim-fmap
add_autocmd('VimEnter', function()
  -- TODO: Implement FNoreMap equivalent
  -- vim.cmd('FNoreMap / ・')
  -- vim.cmd('FNoreMap T ・')
  -- vim.cmd('FNoreMap tt …')
  -- vim.cmd("FNoreMap '' 　")
  -- vim.cmd('FNoreMap p （')
  -- vim.cmd('FNoreMap k 「')
  -- vim.cmd('FNoreMap K 〈')
  -- vim.cmd('FNoreMap -k 『')
end)

-- vim-indent-guides
add_autocmd({ 'VimEnter', 'ColorScheme' }, function()
  vim.api.nvim_set_hl(0, 'IndentGuidesOdd', { ctermbg = 60, bg = '#468F8C' })
  vim.api.nvim_set_hl(0, 'IndentGuidesEven', { ctermbg = 60, bg = '#468F8C' })
end)

-- TODO: When I move to another window, the terminal buffer also becomes IndentGuidesEnable in the autocmd below
add_autocmd({ 'BufEnter', 'WinEnter' }, function()
  if vim.bo.buftype == 'terminal' then
    -- TODO: Implement IndentGuidesDisable equivalent
    -- vim.cmd('IndentGuidesDisable')
  end
end)

add_autocmd({ 'BufLeave', 'WinLeave' }, function()
  vim.wo.relativenumber = false
  -- TODO: Implement IndentGuidesEnable equivalent
  -- vim.cmd('IndentGuidesEnable')
end)

-- vim-precious
-- TODO: Implement precious plugin equivalent
-- add_autocmd('User', function()
--   vim.cmd('IndentGuidesToggle | IndentGuidesToggle')
-- end, 'PreciousFileType')

-- add_autocmd({'WinEnter', 'BufEnter', 'TabEnter'}, function()
--   vim.cmd('PreciousSwitch')
-- end)

-- vim-cursorword
add_autocmd({ 'VimEnter', 'ColorScheme' }, function()
  vim.api.nvim_set_hl(0, 'CursorWord0', { ctermbg = 'LightGray', ctermfg = 'Black' })
  vim.api.nvim_set_hl(0, 'CursorWord1', { ctermbg = 'LightGray', ctermfg = 'Black' })
end)

-- AsyncRun
-- TODO: Implement popup_atcursor equivalent
-- add_autocmd('User', function()
--   vim.call('vimrc#popup_atcursor', ':AsyncRun finished')
-- end, 'AsyncRunStop')

-- ddu.vim
-- boolean | string
vim.g.vimrc_ddu_start_with_insert_next = false

local function get_feedkeys_for_ddu_start()
  if type(vim.g.vimrc_ddu_start_with_insert_next) == 'string' then
    return 'i' .. vim.g.vimrc_ddu_start_with_insert_next
  end
  if vim.g.vimrc_ddu_start_with_insert_next then
    return 'i'
  end
  error('Nothing feedkeys')
end

-- When the next ddu ready, ddu starts from insert
-- TODO: Implement ddu equivalent
-- add_autocmd('User', function()
--   if vim.g.vimrc_ddu_start_with_insert_next ~= false then
--     vim.fn.feedkeys(get_feedkeys_for_ddu_start())
--     vim.g.vimrc_ddu_start_with_insert_next = false
--   end
-- end, 'Ddu:uiReady')
