---A key to spawn claudecode.nvim
local toggle_key = '<leader>cc'

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
    terminal = { -- Open in a floating window
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
  },
}
