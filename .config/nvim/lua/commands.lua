local c = require('chotto')
local docker = require('docker')
local git = require('git')
local nvim = require('nvim')
local nodejs = require('nodejs')
local s = require('utils.functions').s
local telescope = require('telescope.builtin')

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
  local ok, result = pcall(nvim.get_current_buffer_dir, InitLua.git_root)
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
  nvim.rename_to(opts.args)
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

create_command('ClaudeCodeDocker', function(opts)
  docker.start_dangerous_claude_code(opts.args)
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

create_command('LuaSnipReload', function()
  nvim.load_luasnips({ reload = true })
end, {
  desc = 'Reload LuaSnip snippets',
})

vim.api.nvim_create_user_command('RemoveTrailingSpacesWithForce', function(opts)
  local range = opts.range > 0 and { opts.line1, opts.line2 } or nil
  nvim.remove_trailing_spaces(true, range)
end, { range = true })

--}}}
