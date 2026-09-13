---A key to spawn claudecode.nvim
local toggle_key = '<leader>cc'

---Routes Claude Code through a locally running headroom proxy, if headroom is installed
---Equivalent to `load-my-env headroom`; see the headroom section in ~/.dotfiles/bash-toys/sources/load-my-env.sh
---Note that /remote-control becomes unavailable while ANTHROPIC_BASE_URL points at the proxy
---@return table<string, string> --Empty when headroom is not executable
local function headroom_env()
  if vim.fn.executable('headroom') ~= 1 then
    return {}
  end

  local host = '127.0.0.1'
  local port = '8787'

  return {
    HEADROOM_HOST = host,
    HEADROOM_PORT = port,
    HEADROOM_MODE = 'cache',
    HEADROOM_BACKEND = 'anthropic',
    HEADROOM_TELEMETRY = 'off',
    ANTHROPIC_BASE_URL = ('http://%s:%s'):format(host, port),
    ENABLE_TOOL_SEARCH = 'true',
  }
end

return {
  'coder/claudecode.nvim',
  dependencies = { 'folke/snacks.nvim' },
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
    { toggle_key, mode = { 'n' }, '<Cmd>ClaudeCodeFocus<CR>', desc = 'Toggle Claude Code' },
    { '<leader>cr', mode = { 'n' }, '<Cmd>ClaudeCode --resume<CR>', desc = 'Resume Claude' },
    { '<leader>cC', mode = { 'n' }, '<Cmd>ClaudeCode<CR>', desc = 'New Claude' },
    { '<leader>cD', mode = { 'n' }, '<Cmd>ClaudeCodeDocker<CR>', desc = 'Toggle Docker Claude' },
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
    git_repo_cwd = true, -- Top-level aliases are supported and forwarded to terminal config
    env = headroom_env(),
    terminal = { -- Open in a floating window
      ---@module 'snacks'
      ---@type snacks.win.Config | {}
      snacks_win_opts = {
        position = 'float',
        width = 0.9,
        height = 0.9,
        border = 'rounded',
        keys = {
          claude_refresh = {
            '<C-r>',
            function(self)
              self:hide()
              vim.schedule(function()
                require('claudecode.terminal').open()
              end)
            end,
            mode = 'n',
            desc = 'Refresh Claude Code Window',
          },
        },
      },
    },
  },
}
