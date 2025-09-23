local helper = require('helper')
local git = require('git')
local nodejs = require('nodejs')
local s = require('utils.functions').s

---@param cmd_name string
---@param func string | function --実行されるVimコマンド、もしくは処理
---@param options? vim.api.keyset.user_command
local function create_command(cmd_name, func, options)
  -- with default options
  options = vim.tbl_extend('keep', options or {}, {
    bar = true,
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
-- Git Utils {{{

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

create_command('Weblio', function(opts)
  vim.cmd(s('WebpageShow weblio {args}', { args = opts.args }))
end, { nargs = '+' })

create_command('Stackage', function(opts)
  vim.cmd(s('WebpageShow stackage {args}', { args = opts.args }))
end, { nargs = '+' })

-- }}}
-- Change pwd {{{

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

-- Clear quickfix
create_command('CClear', function()
  vim.fn.setqflist({})
end)

-- Rename a file of the current buffer
create_command('Rename', function(opts)
  helper.rename_to(opts.args)
end, { nargs = 1, complete = 'file' })

-- TODO: Not working well
-- Rename variables by lsp
create_command('LspRename', vim.lsp.buf.references)

create_command('LspFormat', function()
  vim.lsp.buf.format({ async = true })
end)

-- TODO: 普通にautofixプラグインを使う（aleあたり）
create_command('KtlintAutoFix', function()
  local current_file = vim.fn.fnameescape(vim.fn.expand('%'))
  vim.fn.system(s('ktlint --format {current_file}', { current_file = current_file }))
  vim.cmd('edit %')
end)

---@param search_word string
create_command('Grep', function(search_word)
  helper.ddu_start_from_input({
    sources = { {
      name = 'rg',
      options = {
        matchers = {},
        volatile = true,
      },
    } },
    uiParams = {
      ff = {
        startFilter = true,
        ignoreEmpty = false,
        autoResize = false,
      },
    },
  }, search_word)
end, { nargs = '?' })

create_command('ReverseLines', '!tac')

create_command('ReplaceListSign', function()
  vim.cmd("'<,'>s/\\(\\s*\\)- /\\1・ /")
end, { range = true })

-- deepl.vim
create_command('DeeplTranslateToEn', function(opts)
  helper.deepl_translate(opts.count, opts.line1, opts.line2, 'EN', 'JA', { 'yank', 'echo' })
end, { range = '%' })

create_command('DeeplTranslateToJa', function(opts)
  helper.deepl_translate(opts.count, opts.line1, opts.line2, 'JA', 'EN', { 'yank', 'echo' })
end, { range = '%' })

create_command('DeeplTranslateToEnOpenBuffer', function(opts)
  helper.deepl_translate(opts.count, opts.line1, opts.line2, 'EN', 'JA', { 'yank', 'buffer' })
end, { range = '%' })

create_command('DeeplTranslateToJaOpenBuffer', function(opts)
  helper.deepl_translate(opts.count, opts.line1, opts.line2, 'JA', 'EN', { 'yank', 'buffer' })
end, { range = '%' })

-- Helper for claudecode.nvim
create_command('ClaudeCodeAtGitRoot', function(opts)
  local git_root = InitLua.git_root
  if not git_root then
    vim.notify('Git root not found!', vim.log.levels.ERROR)
    return
  end

  local current_dir = vim.fn.getcwd()
  vim.cmd('lcd ' .. git_root)
  vim.cmd('ClaudeCode ' .. opts.args)
  vim.cmd('lcd ' .. current_dir)
end, {
  nargs = '*',
  desc = [[
    Open ClaudeCode at git root directory with a floating window.
    Useful when wanting to use a conversation related with gir-root.
    e.g., --continue, --resume.
  ]],
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

---Tapis functions
function Tapi_Tabnew(_, args)
  local files = vim.list_slice(args, 2) -- Skip first element (equivalent to args[1:])
  local paths = vim.tbl_map(vim.fn.fnameescape, files)

  for _, path in ipairs(paths) do
    vim.cmd(s('tabnew {path}', { path = path }))
  end
end

function Tapi_Verticalnew(_, args)
  local files = vim.list_slice(args, 2) -- Skip first element (equivalent to args[1:])
  local paths = vim.tbl_map(vim.fn.fnameescape, files)

  for _, path in ipairs(paths) do
    vim.cmd(s('vertical new {path}', { path = path }))
  end
end
