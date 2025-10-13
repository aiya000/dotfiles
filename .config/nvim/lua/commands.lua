local Terminal = require('toggleterm.terminal').Terminal
local c = require('chotto')
local git = require('git')
local helper = require('helper')
local nodejs = require('nodejs')
local s = require('utils.functions').s
local telescope = require('telescope.builtin')

-- TODO: 型が参照できていないのを直す。たぶん.luarrc.jsonに`~/.luarocks/share/lua/5.1`あたりを指定すればいける？
---@type chotto.Schema<Nargs>
local nargs_schema = c.optional(
  c.union({
    c.literal('*'),
    c.literal('+'),
    c.literal('?'),
    c.literal(1), -- `'1'` of string throws error! fxxx
  })
)

---@param cmd_name string
---@param func string | function --実行されるVimコマンド、もしくは処理
---@param options? vim.api.keyset.user_command
local function create_command(cmd_name, func, options)
  local nargs = nargs_schema:parse((options or {}).nargs) ---@type Nargs
  options = vim.tbl_extend('keep', options or {}, {
    bar = true,
    nargs = nargs,
  })
  vim.api.nvim_create_user_command(cmd_name, func, options)
end

local function read_node_base_dir()
  local ok, result = pcall(helper.get_current_buffer_dir, InitLua.git_root)
  return ok and result or InitLua.path_at_started or error('No base directory found')
end

-- Neovim echo systems {{{

create_command('FtpluginEditAfter', function(opts)
  local filetype = opts.args ~= '' and opts.args or vim.bo.filetype
  local path =
    s('{neovim_home}/after/ftplugin/{filetype}.lua', { neovim_home = InitLua.neovim_home, filetype = filetype })
  vim.cmd(s('edit {path}', { path = path }))
end, { nargs = '?', complete = 'filetype' })

create_command('FtDetectEdit', function(opts)
  local filetype = opts.args ~= '' and opts.args or vim.bo.filetype
  vim.cmd(s('edit {neovim_home}/ftdetect/{filetype}.vim', { neovim_home = InitLua.neovim_home, filetype = filetype }))
end, { nargs = '?', complete = 'filetype' })

-- }}}
-- git utils {{{

create_command('GitCommitFixup', function(opts)
  local commit_hash = opts.args
  local result = vim.system({ 'git', 'commit', '--fixup', commit_hash }):wait()
  if result.code == 0 then
    vim.notify('A fixup commit created for: ' .. commit_hash, vim.log.levels.INFO)
  else
    vim.notify('Failed to create a fixup commit: ' .. (result.stderr or 'Unknown error'), vim.log.levels.ERROR)
  end
end, { nargs = 1 })

create_command('GitTree', function(opts)
  vim.cmd('GinLog --graph --decorate --oneline ' .. vim.fn.string(opts.args))
end, { nargs = '*' })

create_command('GitTreeAll', function(opts)
  vim.cmd('GinLog --graph --decorate --oneline --all' .. opts.args)
end, { nargs = '*' })

-- }}}
-- vim-webpage {{{

create_command('Stackage', function(opts)
  vim.cmd('WebpageShow stackage ' .. opts.args )
end, { nargs = '+' })

-- }}}
-- cd utils {{{

-- :cd
create_command('CdBufDir', function()
  local buf_dir = vim.fn.fnameescape(vim.fn.expand('%:p:h'))
  vim.cmd(s('cd {buf_dir}', { buf_dir = buf_dir }))
end)

create_command('CdStarted', function()
  vim.cmd(s('cd {path_at_started}', { path_at_started = InitLua.path_at_started }))
end)

create_command('CdGitRoot', function()
  git.cd_git_root('cd')
end)

create_command('CdNodeRoot', function()
  nodejs.cd_node_root('cd', read_node_base_dir())
end)

-- :lcd
create_command('LcdBufDir', function()
  local buf_dir = vim.fn.fnameescape(vim.fn.expand('%:p:h'))
  vim.cmd(s('lcd {buf_dir}', { buf_dir = buf_dir }))
end)

create_command('LcdStarted', function()
  vim.cmd(s('lcd {path_at_started}', { path_at_started = InitLua.path_at_started }))
end)

create_command('LcdGitRoot', function()
  git.cd_git_root('lcd')
end)

create_command('LcdNodeRoot', function()
  nodejs.cd_node_root('lcd', read_node_base_dir())
end)

-- :tcd
create_command('TcdBufDir', function()
  local buf_dir = vim.fn.fnameescape(vim.fn.expand('%:p:h'))
  vim.cmd(s('tcd {buf_dir}', { buf_dir = buf_dir }))
end)

create_command('TcdStarted', function()
  vim.cmd(s('tcd {path_at_started}', { path_at_started = InitLua.path_at_started }))
end)

create_command('TcdGitRoot', function()
  git.cd_git_root('tcd')
end)

create_command('TcdNodeRoot', function()
  nodejs.cd_node_root('tcd', read_node_base_dir())
end)

-- g:vimrc.path_at_started assignment
create_command('ScdBufDir', function()
  InitLua.path_at_started = vim.fn.expand('%:p:h')
end)

create_command('ScdCurrentDir', function()
  InitLua.path_at_started = vim.fn.getcwd()
end)

create_command('ScdGitRoot', function()
  InitLua.path_at_started = InitLua.git_root
end)

create_command('ScdNodeRoot', function()
  -- TODO: Implement equivalent for s:Msg.error
  vim.cmd('echohl ErrorMsg | echo "Not implemented yet" | echohl None')
end)

-- }}}
-- Others {{{

create_command('CClear', function()
  vim.fn.setqflist({})
end, { desc = 'Clear quickfix' })

create_command('Rename', function(opts)
  helper.rename_to(opts.args)
end, { nargs = 1, complete = 'file', desc = 'Rename current file to the new name' })

-- TODO: ちゃんと動いてる？
create_command('LspRename', function(opts)
  vim.lsp.buf.rename(opts.args)
end, { nargs = 1, desc = 'Rename symbol under cursor to the new name' })

create_command('LspFormat', function()
  vim.lsp.buf.format({ async = true })
end)

-- TODO: 普通にautofixプラグインを使う（aleあたり）
create_command('KtlintAutoFix', function()
  local current_file = vim.fn.fnameescape(vim.fn.expand('%'))
  vim.fn.system(s('ktlint --format {current_file}', { current_file = current_file }))
  vim.cmd('edit %')
end)

create_command('Grep', function(opts)
  telescope.grep_string({ search = opts.args })
end, { nargs = 1 })

create_command('ReverseLines', '!tac', {
  range = true,
  desc = 'Reverse the order of lines in the selected range or entire buffer',
})

create_command('ClaudeCodeAtGitRoot', function(opts)
  git.execute_cmd_at_git_root('ClaudeCode', opts.args or '')
end, {
  nargs = '*',
  desc = 'Open ClaudeCode at git root directory',
})

create_command('ClaudeCodeAddAtGitRoot', function(opts)
  git.execute_cmd_at_git_root('ClaudeCodeAdd', opts.args or '')
end, {
  nargs = '*',
  desc = 'Add file to Claude Code context at git root',
})

create_command('ClaudeCodeOpenAtGitRoot', function(opts)
  git.execute_cmd_at_git_root('ClaudeCodeOpen', opts.args or '')
end, {
  nargs = '*',
  desc = 'Open Claude Code terminal at git root',
})

create_command('ClaudeCodeSendAtGitRoot', function(opts)
  git.execute_cmd_at_git_root('ClaudeCodeSend', opts.args or '')
end, {
  nargs = '*',
  range = true,
  desc = 'Send selection to Claude Code at git root',
})

create_command('ClaudeCodeFocusAtGitRoot', function(opts)
  git.execute_cmd_at_git_root('ClaudeCodeFocus', opts.args or '')
end, {
  nargs = '*',
  desc = 'Focus Claude Code terminal at git root',
})

ClaudeDockerTerminal = nil

---@param git_root string
---@param image_name string
---@param args string
local function start_creating_claude_docker_terminal(git_root, image_name, args)
  -- Show build progress in floating window
  local cmd = s('docker build -t {image_name} {git_root}/docker/claude-code', {
    image_name = image_name,
    git_root = git_root,
  })

  -- TODO: Not working?
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
        vim.cmd('ClaudeCodeDockerAtGitRoot ' .. args)
      end, 1000)
    end,
  }):toggle()
end

---@param git_root string
---@param image_name string
---@param args string
local function run_claude_docker_terminal(git_root, image_name, args)
  local cmd = s(
    'docker run -it --rm -v {git_root}:/workspace -v claude-code-config:/home/vscode/.claude -e CLAUDE_CONFIG_DIR=/home/vscode/.claude -w /workspace {image_name} claude --dangerously-skip-permissions {args}',
    {
      git_root = vim.fn.shellescape(git_root),
      image_name = image_name,
      args = args,
    }
  )

  -- Create and store terminal instance
  ClaudeDockerTerminal = Terminal:new({
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
      ClaudeDockerTerminal = nil
      vim.notify('Claude Code Docker terminal closed', vim.log.levels.INFO)
    end,
  })

  ClaudeDockerTerminal:toggle()
end

create_command('ClaudeCodeDockerAtGitRoot', function(opts)
  local git_root = InitLua.git_root
  if not git_root then
    vim.notify('Git root not found!', vim.log.levels.ERROR)
    return
  end

  -- Show ClaudeDockerTerminal when it is still running
  if ClaudeDockerTerminal ~= nil then
    ClaudeDockerTerminal:toggle()
    return
  end
  local args = opts.args or ''
  local image_name = 'dotfiles-claude-code:latest'

  -- Check if image exists, if not build it
  local check_image = vim.system({ 'docker', 'images', '-q', image_name }):wait()
  if check_image.code ~= 0 then
    vim.notify('Failed to check Docker images: ' .. (check_image.stderr or 'Unknown error'), vim.log.levels.ERROR)
    return
  end

  if check_image.stdout == '' then
    start_creating_claude_docker_terminal(git_root, image_name, args)
  end
  run_claude_docker_terminal(git_root, image_name, args)
end, {
  nargs = '*',
  desc = 'Open ClaudeCode in Docker container at git root (auto-builds image if needed)',
})

create_command('LuaSnipEdit', function(opts)
  local filetype = opts.args ~= '' and opts.args or vim.bo.filetype
  if filetype == '' then
    vim.notify('No filetype specified or detected', vim.log.levels.WARN)
    return
  end

  local luasnip_path = ('%s/lua/luasnippets/%s.lua'):format(vim.fn.stdpath('config'), filetype)
  vim.cmd('edit ' .. vim.fn.fnameescape(luasnip_path))
end, {
  nargs = '?',
  desc = 'Edit LuaSnip snippet file for current or specified filetype',
  complete = 'filetype',
})

vim.api.nvim_create_user_command('RemoveTrailingSpacesWithForce', function(opts)
  local range = opts.range > 0 and { opts.line1, opts.line2 } or nil
  helper.remove_trailing_spaces(true, range)
end, { range = true })

--}}}
