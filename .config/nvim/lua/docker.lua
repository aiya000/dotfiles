local Terminal = require('toggleterm.terminal').Terminal
local s = require('utils.functions').s

local M = {}

local claude_docker_terminal = nil

---@param image_name string
---@param args string
local function start_dangerous_claude_code(image_name, args)
  -- Show build progress in floating window
  local cmd = s('docker build -t {image_name} ~/.dotfiles/docker/claude-code', {
    image_name = image_name,
  })

  -- Build the image
  Terminal:new({
    cmd = cmd,
    direction = 'float',
    float_opts = {
      border = 'curved',
      width = math.floor(vim.o.columns * 0.8),
      height = math.floor(vim.o.lines * 0.8),
    },

    on_open = function(term)
      vim.cmd('startinsert!')
      -- Set buffer name for clarity
      vim.api.nvim_buf_set_name(term.bufnr, 'Docker Build Progress')
    end,

    on_exit = function(_, _, exit_code, _)
      if exit_code ~= 0 then
        vim.notify('Failed to build Docker image (exit code: ' .. exit_code .. ')', vim.log.levels.ERROR)
        return
      end
      vim.notify('Docker image built successfully! Opening Claude Code...', vim.log.levels.INFO)
      -- Wait a bit then open Claude Code
      vim.defer_fn(function()
        vim.cmd('ClaudeCodeDocker ' .. args)
      end, 1000)
    end,
  }):toggle()
end

---@param project_root string
---@param image_name string
---@param args string
local function run_dangerous_claude_code(project_root, image_name, args)
  local cmd = s(
    'docker run -it --rm -v {project_root}:/workspace -v claude-code-config:/home/vscode/.claude -e CLAUDE_CONFIG_DIR=/home/vscode/.claude -w /workspace {image_name} claude --dangerously-skip-permissions {args}',
    {
      project_root = vim.fn.shellescape(project_root),
      image_name = image_name,
      args = args,
    }
  )

  -- Create and store terminal instance
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
      -- Clear the global terminal instance when it exits
      claude_docker_terminal = nil
      vim.notify('Claude Code Docker terminal closed', vim.log.levels.INFO)
    end,
  })

  claude_docker_terminal:toggle()
end

---@param args? string
function M.start_dangerous_claude_code(args)
  args = args or ''

  -- Show claude_docker_terminal when it is still running
  if claude_docker_terminal ~= nil then
    claude_docker_terminal:toggle()
    return
  end
  local image_name = 'dotfiles-claude-code:latest'

  -- Check if image exists, if not build it
  local check_image = vim.system({ 'docker', 'images', '-q', image_name }):wait()
  if check_image.code ~= 0 then
    vim.notify('Failed to check Docker images: ' .. (check_image.stderr or 'Unknown error'), vim.log.levels.ERROR)
    return
  end

  if check_image.stdout == '' then
    start_dangerous_claude_code(image_name, args)
  end
  local cwd = vim.fn.getcwd() -- TODO: Detect project root?
  run_dangerous_claude_code(cwd, image_name, args)
end

return M
