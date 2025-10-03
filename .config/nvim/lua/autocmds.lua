---特定のプラグインに関連する操作をここに書かないでください。
---我々はこれによって、繰り返し失敗してきました

local helper = require('helper')
local s = require('utils.functions').s

local M = {}

local augroup = vim.api.nvim_create_augroup('InitLuaAutocmds', { clear = true })

-- Move the cursor position to the last position of a file
vim.api.nvim_create_autocmd('BufReadPost', {
  group = augroup,
  callback = function()
    vim.cmd(':' .. vim.fn.line('\'"'))
  end,
})

-- Setup Cmdwin
vim.api.nvim_create_autocmd('CmdwinEnter', {
  group = augroup,
  callback = function()
    vim.keymap.set('n', 'Q', '<Cmd>quit<CR>', { buffer = true, silent = true })
    vim.keymap.set('n', 'ghq', '<Cmd>quit<CR>', { buffer = true, silent = true })
    vim.keymap.set('n', '<C-l>', '<Cmd>quit<CR>', { buffer = true })
    vim.keymap.set('n', 'ghQ', '<Cmd>qall<CR>', { buffer = true, silent = true })
    vim.keymap.set('n', '<C-j>', '<Esc><CR>', { buffer = true })
    vim.keymap.set('n', '<CR>', '<Esc><CR>', { buffer = true })
  end,
})

-- Show :help by vertical split default
vim.api.nvim_create_autocmd('BufWinEnter', {
  group = augroup,
  pattern = { '*.txt' },
  callback = function()
    if vim.bo.filetype == 'help' then
      vim.cmd('wincmd H')
    end
  end,
})

-- Show relative numbers only on the current window {{{

vim.api.nvim_create_autocmd({ 'BufEnter', 'WinEnter' }, {
  group = augroup,
  callback = function()
    if vim.wo.number then
      vim.wo.relativenumber = true
    end
  end,
})

vim.api.nvim_create_autocmd({ 'BufLeave', 'WinLeave' }, {
  group = augroup,
  callback = function()
    vim.wo.relativenumber = false
  end,
})

-- }}}

-- Show full-width spaces
vim.api.nvim_create_autocmd('ColorScheme', {
  group = augroup,
  callback = function()
    vim.api.nvim_set_hl(0, 'EmSpace', { ctermbg = 'LightBlue', bg = 'LightBlue' })
  end,
})

-- TODO: Not working?
vim.api.nvim_create_autocmd({ 'VimEnter', 'WinEnter' }, {
  group = augroup,
  callback = function()
    vim.fn.matchadd('EmSpace', '　')
  end,
})

-- Colorize git conflicts
vim.api.nvim_create_autocmd('ColorScheme', {
  group = augroup,
  callback = function()
    vim.api.nvim_set_hl(0, 'GitConflict', { ctermbg = 'Red', bg = 'Red' })
  end,
})

vim.api.nvim_create_autocmd({ 'VimEnter', 'WinEnter' }, {
  group = augroup,
  callback = function()
    vim.fn.matchadd('GitConflict', [[^\(<\|=\|>\)\{7\}\([^=].\+\)\?$]])
  end,
})

-- Language specific autocmds
local natural_language_filetypes = {
  '',
  'markdown',
  'txt',
}

-- TODO: ここでエラーが出るけど、後でcoc.nvimに置き換えるので、今はコメントアウト
-- > Error detected while processing BufEnter Autocommands for "*":
-- > Error executing lua callback: Vim:E117: Unknown function: ddc#disable
-- > stack traceback:
-- >         [C]: in function 'ddc#disable'
-- >         /home/aiya000/.config/nvim/lua/autocmds.lua:142: in function </home/aiya000/.config/nvim/lua/autocmds.lua:140>
-- -- 自然言語を書いているとddcが重すぎるので、一時的に無効化する
-- add_autocmd({ 'BufEnter', 'WinEnter' }, function()
--   if vim.tbl_contains(natural_language_filetypes, vim.bo.filetype) then
--     vim.fn['ddc#disable']()
--   else
--     vim.fn['ddc#enable']()
--   end
-- end)

-- Scala configuration
vim.api.nvim_create_autocmd('VimEnter', {
  group = augroup,
  callback = function()
    if vim.fn.filereadable('./scalastyle_config.xml') == 1 then
      local answer = vim.fn.input('locally scalastyle_config.xml was found, Do you want to load? (y/n)')
      if answer == 'y' then
        local cwd = vim.fn.getcwd()
        vim.g.ale_scala_scalastyle_config = s('{cwd}/scalastyle-config.xml', { cwd = cwd })
        vim.cmd(s('echomsg "a scalastyle config loaded: {config}"', { config = vim.g.ale_scala_scalastyle_config }))
      end
    end
  end,
})

-- cmdpalette.nvimと連携するための、疑似的な`nnoremap ::: :<C-u>%s/`
-- See 'cmdpalette'.nvim section in './plugins.lua'
vim.api.nvim_create_autocmd('CmdlineChanged', {
  group = augroup,
  callback = function()
    helper.replace_line(
      {
        ['::'] = '%s/',
      },
      vim.fn.getcmdline(),
      vim.fn.setcmdline
    )
  end,
})

-- vim-fmap {{{

vim.api.nvim_create_autocmd('VimEnter', {
  group = augroup,
  callback = function()
    vim.cmd('FNoreMap / ・')
    vim.cmd('FNoreMap T ・')
    vim.cmd('FNoreMap tt …')
    vim.cmd("FNoreMap '' 　")
    vim.cmd('FNoreMap p （')
    vim.cmd('FNoreMap k 「')
    vim.cmd('FNoreMap K 〈')
    vim.cmd('FNoreMap -k 『')
  end,
})

-- }}}
-- vim-precious {{{

vim.api.nvim_create_autocmd({ 'WinEnter', 'BufEnter', 'TabEnter' }, {
  group = augroup,
  callback = function()
    pcall(helper.vim_cmd, 'PreciousSwitch') -- もしvim-preciousがまだ読み込まれていなかったら、何もしない
  end,
})

-- }}}
-- vim-cursorword {{{

vim.api.nvim_create_autocmd({ 'VimEnter', 'ColorScheme' }, {
  group = augroup,
  callback = function()
    vim.api.nvim_set_hl(0, 'CursorWord0', { ctermbg = 'LightGray', ctermfg = 'Black' })
    vim.api.nvim_set_hl(0, 'CursorWord1', { ctermbg = 'LightGray', ctermfg = 'Black' })
  end,
})

-- }}}
-- AsyncRun {{{

-- TODO: vimrc#popup_atcursorがなくなったので、代替に移行する
-- add_autocmd('User', function()
--   vim.call('vimrc#popup_atcursor', ':AsyncRun finished')
-- end, 'AsyncRunStop')

-- }}}

-- }}}

return M
