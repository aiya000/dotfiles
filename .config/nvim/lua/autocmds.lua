-- 自動コマンド設定

local augroup = vim.api.nvim_create_augroup
local autocmd = vim.api.nvim_create_autocmd

-- Avoids nmap onto unterminal buffer
local function nmap_p_to_put_if_on_terminal()
  -- TODO: Implement term_list() equivalent for Neovim
  -- if not vim.tbl_contains(vim.fn.term_list(), vim.fn.winbufnr('.')) then
  --   return
  -- end
  -- TODO: Implement put_as_stdin equivalent
  -- vim.keymap.set('n', 'p', 'vimrc#put_as_stdin(@")', { buffer = true, expr = true })
end

-- Simular to nmap_p_to_put_if_on_terminal()
local function nmap_plus_p_to_put_if_on_terminal()
  -- TODO: Implement term_list() equivalent for Neovim
  -- if not vim.tbl_contains(vim.fn.term_list(), vim.fn.winbufnr('.')) then
  --   return
  -- end
  -- TODO: Implement put_as_stdin equivalent
  -- vim.keymap.set('n', '"+p', 'vimrc#put_as_stdin(@+)', { buffer = true, expr = true })
end

-- See a below autocmd that this is called
local backedup_buftype = ''

local function backup_buftype()
  if vim.bo.buftype == 'nofile' then
    backedup_buftype = 'nofile'
    vim.bo.buftype = ''
  end
end

local function restore_buftype()
  if backedup_buftype == 'nofile' then
    backedup_buftype = ''
    vim.bo.buftype = 'nofile'
  end
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

-- NOTE: 特定のプラグインに関連する操作をここに書かないでください。我々はこれによって、繰り返し失敗してきました
local vimrc_group = augroup('vimrc', { clear = true })

-- TODO: Implement ScdCurrentDir equivalent
-- autocmd('VimEnter', {
--   group = vimrc_group,
--   callback = function() vim.cmd('ScdCurrentDir') end,
-- })

-- TODO: Implement visit_past_position equivalent
-- autocmd('BufReadPost', {
--   group = vimrc_group,
--   callback = function() vim.call('vimrc#visit_past_position') end,
-- })

autocmd('CmdwinEnter', {
  group = vimrc_group,
  callback = setup_cmdwin,
})

-- `buftype=nofile`でddcを動かすと何かが起こったので（なんだっけ？）、一時的に`buftype=`にする
autocmd('InsertEnter', {
  group = vimrc_group,
  callback = backup_buftype,
})

autocmd('InsertLeave', {
  group = vimrc_group,
  callback = restore_buftype,
})

-- TODO: for any registers
-- TODO: Implement terminal mappings for non-nvim

-- Show simply for terminal buffers
autocmd('TermOpen', {
  group = vimrc_group,
  callback = function()
    vim.opt_local.list = false
    vim.opt_local.number = false
    vim.opt_local.relativenumber = false
  end,
})

-- Show relative numbers only on the current window
autocmd({'BufEnter', 'WinEnter'}, {
  group = vimrc_group,
  callback = function()
    if vim.wo.number then
      vim.wo.relativenumber = true
    end
  end,
})

autocmd({'BufLeave', 'WinLeave'}, {
  group = vimrc_group,
  callback = function()
    vim.wo.relativenumber = false
  end,
})

-- Show full-width spaces
autocmd('ColorScheme', {
  group = vimrc_group,
  callback = function()
    vim.api.nvim_set_hl(0, 'EmSpace', { ctermbg = 'LightBlue', bg = 'LightBlue' })
  end,
})

autocmd({'VimEnter', 'WinEnter'}, {
  group = vimrc_group,
  callback = function()
    vim.fn.matchadd('EmSpace', '　')
  end,
})

-- Colorize git conflicts
autocmd('ColorScheme', {
  group = vimrc_group,
  callback = function()
    vim.api.nvim_set_hl(0, 'GitConflict', { ctermbg = 'Red', bg = 'Red' })
  end,
})

autocmd({'VimEnter', 'WinEnter'}, {
  group = vimrc_group,
  callback = function()
    vim.fn.matchadd('GitConflict', [[^\(<\|=\|>\)\{7\}\([^=].\+\)\?$]])
  end,
})

-- StatusLine
autocmd('InsertEnter', {
  group = vimrc_group,
  callback = function()
    vim.api.nvim_set_hl(0, 'StatusLine', { ctermfg = 231, ctermbg = 64 })
  end,
})

autocmd('InsertLeave', {
  group = vimrc_group,
  callback = function()
    vim.api.nvim_set_hl(0, 'StatusLine', { ctermfg = 231, ctermbg = 60 })
  end,
})

-- Hightlight cursors of another windows
autocmd({'BufEnter', 'WinEnter'}, {
  group = vimrc_group,
  callback = function()
    vim.opt_local.cursorline = false
  end,
})

autocmd({'BufLeave', 'WinLeave'}, {
  group = vimrc_group,
  callback = function()
    vim.opt_local.cursorline = true
  end,
})

autocmd({'VimEnter', 'ColorScheme'}, {
  group = vimrc_group,
  callback = function()
    vim.api.nvim_set_hl(0, 'CursorLine', { ctermbg = 60 })
  end,
})

-- Language specific autocmds
local natural_language_filetypes = {
  '',
  'markdown',
  'txt',
}

-- 自然言語を書いているとddcが重すぎるので、一時的に無効化する
autocmd({'BufEnter', 'WinEnter'}, {
  group = vimrc_group,
  callback = function()
    if vim.tbl_contains(natural_language_filetypes, vim.bo.filetype) then
      -- TODO: Implement ddc#disable() equivalent
      -- vim.call('ddc#disable')
    else
      -- TODO: Implement ddc#enable() equivalent
      -- vim.call('ddc#enable')
    end
  end,
})

-- TypeScript/JavaScript specific
local function read_deno_local_tsconfig()
  local local_tsconfig = vim.fn.getcwd() .. '/tsconfig.json'
  if vim.fn.filereadable(local_tsconfig) == 1 then
    vim.g.ale_javascript_deno_lint_options = '--config ' .. local_tsconfig
  end
end

autocmd('FileType', {
  group = vimrc_group,
  pattern = {'typescript', 'javascript'},
  callback = read_deno_local_tsconfig,
})

-- ALE color scheme
autocmd('ColorScheme', {
  group = vimrc_group,
  callback = function()
    vim.api.nvim_set_hl(0, 'ALEError', { ctermbg = 'gray', ctermfg = 'black' })
  end,
})

-- Scala configuration
autocmd('VimEnter', {
  group = vimrc_group,
  callback = function()
    if vim.fn.filereadable('./scalastyle_config.xml') == 1 then
      local answer = vim.fn.input('locally scalastyle_config.xml was found, Do you want to load? (y/n)')
      if answer == 'y' then
        vim.g.ale_scala_scalastyle_config = vim.fn.getcwd() .. '/scalastyle-config.xml'
        vim.cmd('echomsg "a scalastyle config loaded: ' .. vim.g.ale_scala_scalastyle_config .. '"')
      end
    end
  end,
})

-- vim-fmap
autocmd('VimEnter', {
  group = vimrc_group,
  callback = function()
    -- TODO: Implement FNoreMap equivalent
    -- vim.cmd('FNoreMap / ・')
    -- vim.cmd('FNoreMap T ・')
    -- vim.cmd('FNoreMap tt …')
    -- vim.cmd("FNoreMap '' 　")
    -- vim.cmd('FNoreMap p （')
    -- vim.cmd('FNoreMap k 「')
    -- vim.cmd('FNoreMap K 〈')
    -- vim.cmd('FNoreMap -k 『')
  end,
})

-- vim-indent-guides
autocmd({'VimEnter', 'ColorScheme'}, {
  group = vimrc_group,
  callback = function()
    vim.api.nvim_set_hl(0, 'IndentGuidesOdd', { ctermbg = 60, bg = '#468F8C' })
    vim.api.nvim_set_hl(0, 'IndentGuidesEven', { ctermbg = 60, bg = '#468F8C' })
  end,
})

-- TODO: When I move to another window, the terminal buffer also becomes IndentGuidesEnable in the autocmd below
autocmd({'BufEnter', 'WinEnter'}, {
  group = vimrc_group,
  callback = function()
    if vim.bo.buftype == 'terminal' then
      -- TODO: Implement IndentGuidesDisable equivalent
      -- vim.cmd('IndentGuidesDisable')
    end
  end,
})

autocmd({'BufLeave', 'WinLeave'}, {
  group = vimrc_group,
  callback = function()
    vim.wo.relativenumber = false
    -- TODO: Implement IndentGuidesEnable equivalent
    -- vim.cmd('IndentGuidesEnable')
  end,
})

-- vim-precious
-- TODO: Implement precious plugin equivalent
-- autocmd('User', {
--   group = vimrc_group,
--   pattern = 'PreciousFileType',
--   callback = function()
--     vim.cmd('IndentGuidesToggle | IndentGuidesToggle')
--   end,
-- })

-- autocmd({'WinEnter', 'BufEnter', 'TabEnter'}, {
--   group = vimrc_group,
--   callback = function()
--     vim.cmd('PreciousSwitch')
--   end,
-- })

-- vim-cursorword
autocmd({'VimEnter', 'ColorScheme'}, {
  group = vimrc_group,
  callback = function()
    vim.api.nvim_set_hl(0, 'CursorWord0', { ctermbg = 'LightGray', ctermfg = 'Black' })
    vim.api.nvim_set_hl(0, 'CursorWord1', { ctermbg = 'LightGray', ctermfg = 'Black' })
  end,
})

-- AsyncRun
-- TODO: Implement popup_atcursor equivalent
-- autocmd('User', {
--   group = vimrc_group,
--   pattern = 'AsyncRunStop',
--   callback = function()
--     vim.call('vimrc#popup_atcursor', ':AsyncRun finished')
--   end,
-- })

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
-- autocmd('User', {
--   group = vimrc_group,
--   pattern = 'Ddu:uiReady',
--   callback = function()
--     if vim.g.vimrc_ddu_start_with_insert_next ~= false then
--       vim.fn.feedkeys(get_feedkeys_for_ddu_start())
--       vim.g.vimrc_ddu_start_with_insert_next = false
--     end
--   end,
-- })