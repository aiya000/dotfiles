local fn = require('utils.functions')

---A key to spawn claudecode.nvim
local toggle_key = '<leader>cc'

---Checks if window is actually displayed (has height/width)
---@param win integer --An element of `vim.api.nvim_list_wins()`
local function is_win_displayed(win)
  local win_config = vim.api.nvim_win_get_config(win)
  return win_config.hide
  or (win_config.height ~= nil and win_config.height == 0)
  or (win_config.width ~= nil and win_config.width == 0)
end

---Does the buffer typically contain 'claude' in its name ClaudeCode terminal?
---@param win integer --An element of `vim.api.nvim_list_wins()`
local function is_buffer_name_claude(win)
  local buf = vim.api.nvim_win_get_buf(win)
  local bufname = vim.api.nvim_buf_get_name(buf)
  return bufname:match('claude') or vim.bo[buf].filetype == 'claudecode'
end

---NOTE: なぜか`:ClaudeCodeFocus`でfloat windowがトグルしないので、ワークアラウンド
---Checks if ClaudeCode window/buffer is currently visible (not hidden)
local function is_claudecode_window_opening()
  return vim.iter(vim.api.nvim_list_wins())
  :any(function(win) ---@param win integer
    return vim.api.nvim_win_is_valid(win)
    and not is_win_displayed(win)
    and is_buffer_name_claude(win)
  end)
end

local function toggle()
  if is_claudecode_window_opening() then
    vim.cmd('ClaudeCodeFocus')
    return
  end

  local cwd = vim.fn.getcwd()
  fn.try_finally(function()
    vim.cmd('lcd ' .. InitLua.path_at_started)
    vim.cmd('ClaudeCode --continue')
    return nil
  end, function()
    vim.cmd('lcd ' .. cwd)
  end)
end

local function toggle_docker()
  vim.cmd('ClaudeCodeDocker')
end

return {
  'coder/claudecode.nvim',
  dependencies = { 'folke/snacks.nvim' },
  config = true,
  cmd = {
    'ClaudeCode',
    'ClaudeCodeFocus',
    'ClaudeCodeSelectModel',
    'ClaudeCodeAdd',
    'ClaudeCodeSend',
    'ClaudeCodeTreeAdd',
    'ClaudeCodeDiffAccept',
    'ClaudeCodeDiffDeny',
  },
  keys = {
    { toggle_key, mode = { 'n' }, toggle, desc = 'Toggle Claude Code' },
    { '<leader>cr', mode = { 'n' }, '<Cmd>ClaudeCode --resume<CR>', desc = 'Resume Claude' },
    { '<leader>cC', mode = { 'n' }, '<Cmd>ClaudeCode<CR>', desc = 'New Claude' },
    { '<leader>cD', mode = { 'n' }, toggle_docker, desc = 'Toggle Docker Claude' },
    { '<leader>cM', mode = { 'n' }, '<Cmd>ClaudeCodeSelectModel<CR>', desc = 'Select Claude model' },
    { '<leader>cb', mode = { 'n' }, '<Cmd>ClaudeCodeAdd %<CR>', desc = 'Add current buffer' },
    { '<leader>cs', mode = { 'n' }, 'V:ClaudeCodeSend<CR>', desc = 'Send to Claude' },
    { '<leader>cs', mode = { 'v' }, '<Cmd>ClaudeCodeSend<CR>', desc = 'Send to Claude' },
    {
      '<leader>cs',
      '<Cmd>ClaudeCodeTreeAdd<CR>',
      desc = 'Add file',
      ft = { 'NvimTree', 'neo-tree', 'oil', 'minifiles' },
    },
    { '<leader>ca', '<Cmd>ClaudeCodeDiffAccept<CR>', desc = 'Accept diff' },
    { '<leader>cd', '<Cmd>ClaudeCodeDiffDeny<CR>', desc = 'Deny diff' },
  },
  opts = {
    -- Open in a floating window
    terminal = {
      ---@module 'snacks'
      ---@type snacks.win.Config | {}
      snacks_win_opts = {
        position = 'float',
        width = 0.9,
        height = 0.9,
        border = 'rounded',
        keys = {
          claude_hide = {
            toggle_key,
            function(self)
              self:hide()
            end,
            mode = 't',
            desc = 'Hide',
          },
        },
      },
    },
    -- TODO: これちゃんと動いてる？ チェックする
    diff_opts = {
      auto_close_on_accept = true,
      vertical_split = true,
      open_in_current_tab = true,
      keep_terminal_focus = false,
    },
  },
}
