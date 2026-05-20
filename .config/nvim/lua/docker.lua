local Terminal = require('toggleterm.terminal').Terminal
local s = require('utils.functions').s

local M = {}

local claude_docker_terminal = nil

---@param project_root string
---@param args string
local function run_dangerous_claude_code(project_root, args)
  local compose_file = vim.fn.expand('~/.dotfiles/docker/claude-code-dangerously-skip-permissions/docker-compose.yml')
  local cmd = s(
    'docker compose -f {compose_file} run --rm -v {project_root}:/workspace claude-code {args}',
    {
      compose_file = vim.fn.shellescape(compose_file),
      project_root = vim.fn.shellescape(project_root),
      args = args,
    }
  )

  vim.notify('poi: ' .. vim.inspect(cmd), vim.log.levels.INFO)

  claude_docker_terminal = Terminal:new({
    cmd = cmd,
    direction = 'float',
    float_opts = {
      border = 'curved',
      width = math.floor(vim.o.columns * 0.9),
      height = math.floor(vim.o.lines * 0.9),
    },

    on_open = function(_)
      vim.cmd('startinsert!')
    end,

    on_exit = function()
      claude_docker_terminal = nil
      vim.notify('Claude Code Docker terminal closed', vim.log.levels.INFO)
    end,
  })

  claude_docker_terminal:toggle()
end

---@param args? string
function M.start_dangerous_claude_code(args)
  args = args or ''

  if claude_docker_terminal ~= nil then
    claude_docker_terminal:toggle()
    return
  end

  local cwd = vim.fn.getcwd()
  run_dangerous_claude_code(cwd, args)
end

return M
