-- TODO: Import vital functions equivalent for Lua
-- let s:List = vital#vimrc#import('Data.List')
-- let s:Msg = vital#vimrc#import('Vim.Message')

-- Virtual keymaps
vim.keymap.set('n', '<Plug>(vimrc-surround-append-choice)', '<Cmd>call vimrc#append_choose_surround()<CR>', { silent = true })
vim.keymap.set('n', '<Plug>(vimrc-surround-append-choice-wide)', '<Cmd>call vimrc#append_choose_surround_wide()<CR>', { silent = true })
vim.keymap.set('n', '<Plug>(vimrc-surround-delete-mostly-inner)', '<Cmd>call vimrc#delete_mostly_inner_surround()<CR>', { silent = true })
vim.keymap.set('n', '<Plug>(vimrc-surround-replace-mostly-inner)', '<Cmd>call vimrc#replace_mostly_inner_surround()<CR>', { silent = true })

-- Vim systems
-- Scripts
vim.api.nvim_create_user_command('FtpluginEditAfter', function(opts)
  local filetype = opts.args ~= '' and opts.args or vim.bo.filetype
  vim.cmd('edit ' .. string.format('%s/after/ftplugin/%s.vim', vim.g.vimrc.vim_home, filetype))
end, { nargs = '?', complete = 'filetype', bar = true })

vim.api.nvim_create_user_command('FtDictionaryEdit', function(opts)
  local filetype = opts.args ~= '' and opts.args or vim.bo.filetype
  vim.cmd('edit ' .. string.format('%s/dict/filetype/%s.dict', vim.g.vimrc.vim_home, filetype))
end, { nargs = '?', complete = 'filetype', bar = true })

vim.api.nvim_create_user_command('SyntaxEdit', function(opts)
  local filetype = opts.args ~= '' and opts.args or vim.bo.filetype
  vim.cmd('edit ' .. string.format('%s/syntax/%s.vim', vim.g.vimrc.vim_home, filetype))
end, { nargs = '?', complete = 'filetype', bar = true })

vim.api.nvim_create_user_command('IndentEdit', function(opts)
  local filetype = opts.args ~= '' and opts.args or vim.bo.filetype
  vim.cmd('edit ' .. string.format('%s/indent/%s.vim', vim.g.vimrc.vim_home, filetype))
end, { nargs = '?', complete = 'filetype', bar = true })

vim.api.nvim_create_user_command('FtDetectEdit', function(opts)
  local filetype = opts.args ~= '' and opts.args or vim.bo.filetype
  vim.cmd('edit ' .. string.format('%s/ftdetect/%s.vim', vim.g.vimrc.vim_home, filetype))
end, { nargs = '?', complete = 'filetype', bar = true })

vim.api.nvim_create_user_command('PluginEdit', function(opts)
  local filetype = opts.args ~= '' and opts.args or vim.bo.filetype
  vim.cmd('edit ' .. string.format('%s/plugin/%s.vim', vim.g.vimrc.vim_home, filetype))
end, { nargs = '?', complete = 'filetype', bar = true })

vim.api.nvim_create_user_command('AutoloadEdit', function(opts)
  local filetype = opts.args ~= '' and opts.args or vim.bo.filetype
  vim.cmd('edit ' .. string.format('%s/autoload/%s.vim', vim.g.vimrc.vim_home, filetype))
end, { nargs = '?', complete = 'filetype', bar = true })

-- Clear quickfix
vim.api.nvim_create_user_command('CClear', function()
  vim.fn.setqflist({})
end, { bar = true })

-- Rename a file of the current buffer
vim.api.nvim_create_user_command('Rename', function(opts)
  vim.fn['vimrc#rename_to'](opts.args)
end, { nargs = 1, complete = 'file', bar = true })

-- Haskell
vim.api.nvim_create_user_command('HaskDogs', function()
  vim.fn['vimrc#execute_haskdogs_async']()
end, { bar = true })

vim.api.nvim_create_user_command('EtaDogs', function()
  vim.fn['vimrc#execute_haskdogs_in_eta_async']()
end, { bar = true })

-- Kotlin
vim.api.nvim_create_user_command('KtlintAutoFix', function()
  vim.fn.system('ktlint --format ' .. vim.fn.fnameescape(vim.fn.expand('%')))
  vim.cmd('edit %')
end, { bar = true })

vim.api.nvim_create_user_command('QuickfixRunGradle', function(opts)
  vim.fn['vimrc#run_gradle_quickfix'](opts.args)
end, { nargs = '*', bar = true })

-- Scala
vim.api.nvim_create_user_command('QuickfixRunSbtCompileWatch', function(opts)
  vim.fn['vimrc#run_scala_compile_watch_quickfix'](opts.args)
end, { nargs = '*', bar = true })

vim.api.nvim_create_user_command('QuickfixStopSbtCompileWatch', function()
  vim.fn['vimrc#stop_scala_compile_watch_quickfix']()
end, { bar = true })

-- Make
vim.api.nvim_create_user_command('QuickfixRunMake', function(opts)
  vim.fn['vimrc#run_make_quickfix'](opts.args)
end, { nargs = '*', bar = true })

-- Git commands (cushion)
vim.api.nvim_create_user_command('GStatus', function(opts)
  vim.cmd('GinStatus ' .. opts.args)
end, { nargs = '*', bar = true })

vim.api.nvim_create_user_command('GLog', function(opts)
  vim.cmd('GitLogViewer -100 --name-only ' .. opts.args)
end, { nargs = '*', bar = true })

vim.api.nvim_create_user_command('GLogPatch', function(opts)
  vim.cmd('GitLogViewer --patch -100 ' .. opts.args)
end, { nargs = '*', bar = true })

vim.api.nvim_create_user_command('GLogOneline', function(opts)
  vim.cmd('GitLogViewer --oneline ' .. opts.args)
end, { nargs = '*', bar = true })

vim.api.nvim_create_user_command('GDiff', function(opts)
  vim.cmd('GitDiffViewer ' .. opts.args)
end, { nargs = '*', bar = true })

vim.api.nvim_create_user_command('GDS', function(opts)
  vim.cmd('GitDiffViewer --staged ' .. opts.args)
end, { nargs = '*', bar = true })

vim.api.nvim_create_user_command('GDH', function(opts)
  vim.cmd('GitDiffViewer HEAD~ ' .. opts.args)
end, { nargs = '*', bar = true })

vim.api.nvim_create_user_command('GCommit', function(opts)
  vim.cmd('Gin commit --verbose ' .. opts.args)
end, { nargs = '*', bar = true })

vim.api.nvim_create_user_command('GCommitAmmend', function(opts)
  vim.cmd('Gin commit --verbose --amend ' .. opts.args)
end, { nargs = '*', bar = true })

vim.api.nvim_create_user_command('GCommitFixup', function(opts)
  vim.cmd('echomsg system("git commit --fixup " .. ' .. vim.fn.string(opts.args) .. ')')
end, { nargs = 1, bar = true })

vim.api.nvim_create_user_command('GAddPatch', function(opts)
  vim.cmd('terminal git add -p ' .. opts.args)
end, { nargs = '*', bar = true })

vim.api.nvim_create_user_command('GTree', function(opts)
  vim.cmd('GinLog --graph --decorate --oneline ' .. opts.args)
end, { nargs = '*', bar = true })

vim.api.nvim_create_user_command('GTreeAll', function(opts)
  vim.cmd('GinLog --graph --decorate --oneline --all ' .. opts.args)
end, { nargs = '*', bar = true })

vim.api.nvim_create_user_command('GBrahcnAll', function(opts)
  vim.cmd('GinBranch --all ' .. opts.args)
end, { nargs = '*', bar = true })

vim.api.nvim_create_user_command('GBlame', function(opts)
  vim.cmd('Gin blame ' .. opts.args)
end, { nargs = '*', bar = true })

-- vim-webpage
vim.api.nvim_create_user_command('Weblio', function(opts)
  vim.cmd('WebpageShow weblio ' .. opts.args)
end, { nargs = '+', bar = true })

vim.api.nvim_create_user_command('Stackage', function(opts)
  vim.cmd('WebpageShow stackage ' .. opts.args)
end, { nargs = '+' })

-- Change directory
-- :cd
vim.api.nvim_create_user_command('CdBufDir', function()
  vim.cmd('cd ' .. vim.fn.fnameescape(vim.fn.expand('%:p:h')))
end, { bar = true })

vim.api.nvim_create_user_command('CdStarted', function()
  vim.cmd('cd ' .. vim.g.vimrc.path_at_started)
end, { bar = true })

vim.api.nvim_create_user_command('CdGitRoot', function()
  vim.fn['vimrc#cd_git_root'](':cd')
end, { bar = true })

vim.api.nvim_create_user_command('CdNodeRoot', function()
  vim.fn['vimrc#cd_node_root'](':cd')
end, { bar = true })

-- :lcd
vim.api.nvim_create_user_command('LcdBufDir', function()
  vim.cmd('lcd ' .. vim.fn.fnameescape(vim.fn.expand('%:p:h')))
end, { bar = true })

vim.api.nvim_create_user_command('LcdStarted', function()
  vim.cmd('lcd ' .. vim.g.vimrc.path_at_started)
end, { bar = true })

vim.api.nvim_create_user_command('LcdGitRoot', function()
  vim.fn['vimrc#cd_git_root'](':lcd')
end, { bar = true })

vim.api.nvim_create_user_command('LcdNodeRoot', function()
  vim.fn['vimrc#cd_node_root'](':lcd')
end, { bar = true })

-- :tcd
vim.api.nvim_create_user_command('TcdBufDir', function()
  vim.cmd('tcd ' .. vim.fn.fnameescape(vim.fn.expand('%:p:h')))
end, { bar = true })

vim.api.nvim_create_user_command('TcdStarted', function()
  vim.cmd('tcd ' .. vim.g.vimrc.path_at_started)
end, { bar = true })

vim.api.nvim_create_user_command('TcdGitRoot', function()
  vim.fn['vimrc#cd_git_root'](':tcd')
end, { bar = true })

vim.api.nvim_create_user_command('TcdNodeRoot', function()
  vim.fn['vimrc#cd_node_root'](':tcd')
end, { bar = true })

-- g:vimrc.path_at_started assignment
vim.api.nvim_create_user_command('ScdBufDir', function()
  vim.g.vimrc.path_at_started = vim.fn.expand('%:p:h')
end, { bar = true })

vim.api.nvim_create_user_command('ScdCurrentDir', function()
  vim.g.vimrc.path_at_started = vim.fn.getcwd()
end, { bar = true })

vim.api.nvim_create_user_command('ScdGitRoot', function()
  vim.g.vimrc.path_at_started = vim.g.vimrc.git_root
end, { bar = true })

vim.api.nvim_create_user_command('ScdNodeRoot', function()
  -- TODO: Implement equivalent for s:Msg.error
  vim.cmd('echohl ErrorMsg | echo "Not implemented yet" | echohl None')
end, { bar = true })

-- Others
vim.api.nvim_create_user_command('GitReadRoot', function()
  vim.fn['vimrc#read_to_set_git_root']()
end, { bar = true })

vim.api.nvim_create_user_command('ReadGitRoot', function()
  vim.fn['vimrc#read_to_set_git_root']()
end, { bar = true })

vim.api.nvim_create_user_command('Grep', function(...)
  local args = {...}
  -- TODO: Implement ddu_start_from_input equivalent
  vim.fn['vimrc#ddu_start_from_input']({
    sources = {{
      name = 'rg',
      options = {
        matchers = {},
        volatile = true,
      },
    }},
    uiParams = {
      ff = {
        startFilter = true,
        ignoreEmpty = false,
        autoResize = false,
      },
    },
  }, unpack(args))
end, { nargs = '?', bar = true })

vim.api.nvim_create_user_command('SetTabTitle', function(opts)
  vim.t.vimrc_tabtitle = opts.args
end, { nargs = '+', bar = true })

vim.api.nvim_create_user_command('UnsetTabTitle', function()
  vim.t.vimrc_tabtitle = nil
end, { bar = true })

vim.api.nvim_create_user_command('ReverseLines', '!tac', { bar = true })

vim.api.nvim_create_user_command('ReplaceListSign', function()
  vim.cmd("'<,'>s/\\(\\s*\\)- /\\1・ /")
end, { range = true, bar = true })

-- deepl.vim
vim.api.nvim_create_user_command('DeeplTranslateToEn', function(opts)
  vim.fn['vimrc#deepl_translate'](opts.count, opts.line1, opts.line2, 'EN', 'JA', {'yank', 'echo'})
end, { range = '%', bar = true })

vim.api.nvim_create_user_command('DeeplTranslateToJa', function(opts)
  vim.fn['vimrc#deepl_translate'](opts.count, opts.line1, opts.line2, 'JA', 'EN', {'yank', 'echo'})
end, { range = '%', bar = true })

vim.api.nvim_create_user_command('DeeplTranslateToEnOpenBuffer', function(opts)
  vim.fn['vimrc#deepl_translate'](opts.count, opts.line1, opts.line2, 'EN', 'JA', {'yank', 'buffer'})
end, { range = '%', bar = true })

vim.api.nvim_create_user_command('DeeplTranslateToJaOpenBuffer', function(opts)
  vim.fn['vimrc#deepl_translate'](opts.count, opts.line1, opts.line2, 'JA', 'EN', {'yank', 'buffer'})
end, { range = '%', bar = true })

-- Tapis functions
function Tapi_Tabnew(_, args)
  local files = vim.list_slice(args, 2)  -- Skip first element (equivalent to args[1:])
  local paths = vim.tbl_map(vim.fn.fnameescape, files)
  
  for _, path in ipairs(paths) do
    vim.cmd('tabnew ' .. path)
  end
end

function Tapi_Verticalnew(_, args)
  local files = vim.list_slice(args, 2)  -- Skip first element (equivalent to args[1:])
  local paths = vim.tbl_map(vim.fn.fnameescape, files)
  
  for _, path in ipairs(paths) do
    vim.cmd('vertical new ' .. path)
  end
end

-- Debug message
-- print("📋 vimrc.lua plugin loaded with all commands and functions")