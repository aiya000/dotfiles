local helper = require('helper')
local git = require('git')
local nodejs = require('nodejs')
local s = require('utils.functions').s

---@class CreateCommandOptions
---@field nargs? 0 | 1 | '*' | '?' | '+'
---@field bar? boolean
---@field complete? string | function --補完に表示するリスト種類か、リストを返す関数

---ちょっと型を明示してみた。
---ちゃんとinit.lua用のプラグイン使えばいらないかも。
---- TODO: いらなかったら削除して、普通に`local create_command = vim.api.nvim_create_user_command`とかする
---@param name string --コマンド名
---@param command string|function --実行されるVimコマンド、もしくは処理
---@param options? CreateCommandOptions
local function create_command(name, command, options)
  vim.api.nvim_create_user_command(name, command, options)
end

-- Neovim echo systems {{{

create_command('FtpluginEditAfter', function(opts)
  local filetype = opts.args ~= '' and opts.args or vim.bo.filetype
  local path = s('{neovim_home}/after/ftplugin/{filetype}.lua', { neovim_home = InitLua.neovim_home, filetype = filetype })
  vim.cmd(s('edit {path}', { path = path }))
end, { nargs = '?', complete = 'filetype', bar = true })

create_command('FtDictionaryEdit', function(opts)
  local filetype = opts.args ~= '' and opts.args or vim.bo.filetype
  vim.cmd(s('edit {neovim_home}/dict/filetype/{filetype}.dict', { neovim_home = InitLua.neovim_home, filetype = filetype }))
end, { nargs = '?', complete = 'filetype', bar = true })

create_command('SyntaxEdit', function(opts)
  local filetype = opts.args ~= '' and opts.args or vim.bo.filetype
  vim.cmd(s('edit {neovim_home}/syntax/{filetype}.vim', { neovim_home = InitLua.neovim_home, filetype = filetype }))
end, { nargs = '?', complete = 'filetype', bar = true })

create_command('IndentEdit', function(opts)
  local filetype = opts.args ~= '' and opts.args or vim.bo.filetype
  vim.cmd(s('edit {neovim_home}/indent/{filetype}.vim', { neovim_home = InitLua.neovim_home, filetype = filetype }))
end, { nargs = '?', complete = 'filetype', bar = true })

create_command('FtDetectEdit', function(opts)
  local filetype = opts.args ~= '' and opts.args or vim.bo.filetype
  vim.cmd(s('edit {neovim_home}/ftdetect/{filetype}.vim', { neovim_home = InitLua.neovim_home, filetype = filetype }))
end, { nargs = '?', complete = 'filetype', bar = true })

create_command('PluginEdit', function(opts)
  local filetype = opts.args ~= '' and opts.args or vim.bo.filetype
  vim.cmd(s('edit {neovim_home}/plugin/{filetype}.vim', { neovim_home = InitLua.neovim_home, filetype = filetype }))
end, { nargs = '?', complete = 'filetype', bar = true })

create_command('AutoloadEdit', function(opts)
  local filetype = opts.args ~= '' and opts.args or vim.bo.filetype
  vim.cmd(s('edit {neovim_home}/autoload/{filetype}.vim', { neovim_home = InitLua.neovim_home, filetype = filetype }))
end, { nargs = '?', complete = 'filetype', bar = true })

-- }}}
-- Cushion commands for git {{{

create_command('GStatus', function(opts)
  vim.cmd(s('GinStatus {args}', { args = opts.args }))
end, { nargs = '*', bar = true })

create_command('GLog', function(opts)
  vim.cmd(s('GitLogViewer -100 --name-only {args}', { args = opts.args }))
end, { nargs = '*', bar = true })

create_command('GLogPatch', function(opts)
  vim.cmd(s('GitLogViewer --patch -100 {args}', { args = opts.args }))
end, { nargs = '*', bar = true })

create_command('GLogOneline', function(opts)
  vim.cmd(s('GitLogViewer --oneline {args}', { args = opts.args }))
end, { nargs = '*', bar = true })

create_command('GDiff', function(opts)
  vim.cmd(s('GitDiffViewer {args}', { args = opts.args }))
end, { nargs = '*', bar = true })

create_command('GDS', function(opts)
  vim.cmd(s('GitDiffViewer --staged {args}', { args = opts.args }))
end, { nargs = '*', bar = true })

create_command('GDH', function(opts)
  vim.cmd(s('GitDiffViewer HEAD~ {args}', { args = opts.args }))
end, { nargs = '*', bar = true })

create_command('GCommitFixup', function(opts)
  vim.cmd(s('echomsg system("git commit --fixup " .. {commit_hash})', { commit_hash = vim.fn.string(opts.args) }))
end, { nargs = 1, bar = true })

create_command('GTree', function(opts)
  vim.cmd(s('GinLog --graph --decorate --oneline {args}', { args = opts.args }))
end, { nargs = '*', bar = true })

create_command('GTreeAll', function(opts)
  vim.cmd(s('GinLog --graph --decorate --oneline --all {args}', { args = opts.args }))
end, { nargs = '*', bar = true })

create_command('GBrahcnAll', function(opts)
  vim.cmd(s('GinBranch --all {args}', { args = opts.args }))
end, { nargs = '*', bar = true })

create_command('GBlame', function(opts)
  vim.cmd(s('Gin blame {args}', { args = opts.args }))
end, { nargs = '*', bar = true })

-- }}}
-- vim-webpage {{{

create_command('Weblio', function(opts)
  vim.cmd(s('WebpageShow weblio {args}', { args = opts.args }))
end, { nargs = '+', bar = true })

create_command('Stackage', function(opts)
  vim.cmd(s('WebpageShow stackage {args}', { args = opts.args }))
end, { nargs = '+' })

-- }}}
-- Change pwd {{{

-- :cd
create_command('CdBufDir', function()
  local buf_dir = vim.fn.fnameescape(vim.fn.expand('%:p:h'))
  vim.cmd(s('cd {buf_dir}', { buf_dir = buf_dir }))
end, { bar = true })

create_command('CdStarted', function()
  vim.cmd(s('cd {path_at_started}', { path_at_started = InitLua.path_at_started }))
end, { bar = true })

create_command('CdGitRoot', function()
  git.cd_git_root('cd')
end, { bar = true })

create_command('CdNodeRoot', function()
  nodejs.cd_node_root('cd')
end, { bar = true })

-- :lcd
create_command('LcdBufDir', function()
  local buf_dir = vim.fn.fnameescape(vim.fn.expand('%:p:h'))
  vim.cmd(s('lcd {buf_dir}', { buf_dir = buf_dir }))
end, { bar = true })

create_command('LcdStarted', function()
  vim.cmd(s('lcd {path_at_started}', { path_at_started = InitLua.path_at_started }))
end, { bar = true })

create_command('LcdGitRoot', function()
  git.cd_git_root('lcd')
end, { bar = true })

create_command('LcdNodeRoot', function()
  nodejs.cd_node_root('lcd')
end, { bar = true })

-- :tcd
create_command('TcdBufDir', function()
  local buf_dir = vim.fn.fnameescape(vim.fn.expand('%:p:h'))
  vim.cmd(s('tcd {buf_dir}', { buf_dir = buf_dir }))
end, { bar = true })

create_command('TcdStarted', function()
  vim.cmd(s('tcd {path_at_started}', { path_at_started = InitLua.path_at_started }))
end, { bar = true })

create_command('TcdGitRoot', function()
  git.cd_git_root('tcd')
end, { bar = true })

create_command('TcdNodeRoot', function()
  nodejs.cd_node_root('tcd')
end, { bar = true })

-- g:vimrc.path_at_started assignment
create_command('ScdBufDir', function()
  InitLua.path_at_started = vim.fn.expand('%:p:h')
end, { bar = true })

create_command('ScdCurrentDir', function()
  InitLua.path_at_started = vim.fn.getcwd()
end, { bar = true })

create_command('ScdGitRoot', function()
  InitLua.path_at_started = InitLua.git_root
end, { bar = true })

create_command('ScdNodeRoot', function()
  -- TODO: Implement equivalent for s:Msg.error
  vim.cmd('echohl ErrorMsg | echo "Not implemented yet" | echohl None')
end, { bar = true })

-- }}}

-- Clear quickfix
create_command('CClear', function()
  vim.fn.setqflist({})
end, { bar = true })

-- Rename a file of the current buffer
create_command('Rename', function(opts)
  helper.rename_to(opts.args)
end, { nargs = 1, complete = 'file', bar = true })

-- TODO: 普通にautofixプラグインを使う（aleあたり）
create_command('KtlintAutoFix', function()
  local current_file = vim.fn.fnameescape(vim.fn.expand('%'))
  vim.fn.system(s('ktlint --format {current_file}', { current_file = current_file }))
  vim.cmd('edit %')
end, { bar = true })

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
end, { nargs = '?', bar = true })

create_command('ReverseLines', '!tac', { bar = true })

create_command('ReplaceListSign', function()
  vim.cmd("'<,'>s/\\(\\s*\\)- /\\1・ /")
end, { range = true, bar = true })

-- deepl.vim
create_command('DeeplTranslateToEn', function(opts)
  helper.deepl_translate(opts.count, opts.line1, opts.line2, 'EN', 'JA', { 'yank', 'echo' })
end, { range = '%', bar = true })

create_command('DeeplTranslateToJa', function(opts)
  helper.deepl_translate(opts.count, opts.line1, opts.line2, 'JA', 'EN', { 'yank', 'echo' })
end, { range = '%', bar = true })

create_command('DeeplTranslateToEnOpenBuffer', function(opts)
  helper.deepl_translate(opts.count, opts.line1, opts.line2, 'EN', 'JA', { 'yank', 'buffer' })
end, { range = '%', bar = true })

create_command('DeeplTranslateToJaOpenBuffer', function(opts)
  helper.deepl_translate(opts.count, opts.line1, opts.line2, 'JA', 'EN', { 'yank', 'buffer' })
end, { range = '%', bar = true })

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
