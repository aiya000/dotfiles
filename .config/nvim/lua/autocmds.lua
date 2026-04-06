---特定のプラグインに関連する操作をここに書かないでください。
---我々はこれによって、繰り返し失敗してきました

local nvim = require('nvim')

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

-- cmdpalette.nvimと連携するための、疑似的な`nnoremap ::: :<C-u>%s/`
vim.api.nvim_create_autocmd('CmdlineChanged', {
  group = augroup,
  callback = function()
    nvim.replace_line({
      [':'] = '%s/',
    }, vim.fn.getcmdline(), vim.fn.setcmdline)
  end,
})

-- Treesitter error suppression
vim.api.nvim_create_autocmd({ 'TextChanged', 'TextChangedI' }, {
  group = augroup,
  callback = function()
    -- treesitterのハイライト更新をスケジュールして、バッファ操作との競合を避ける
    -- TODO: これでもだめかも。だめだったら修正する。大丈夫そうだったら、このコメントを削除する
    vim.schedule(function()
      if vim.bo.filetype ~= '' then
        pcall(vim.treesitter.get_parser)
      end
    end)
  end,
})

-- Highlighting and ColorSchema {{{

vim.api.nvim_create_autocmd({ 'VimEnter', 'ColorScheme' }, {
  group = augroup,
  callback = function()
    vim.api.nvim_set_hl(0, 'CursorLine', { bg = '#585b70' })
    vim.api.nvim_set_hl(0, 'CursorColumn', { bg = '#585b70', bold = true })
    vim.api.nvim_set_hl(0, 'Visual', { bg = '#6e4a7c' })
    vim.api.nvim_set_hl(0, 'TrailingSpace', { ctermbg = 'Red', bg = '#F38BA8' })

    vim.api.nvim_set_hl(0, 'EmSpace', { ctermbg = 'LightBlue', bg = '#89B4FA' })
    vim.fn.matchadd('EmSpace', '　')

    -- TODO: *.luaで、'TODO:', 'FIXME:', 'NOTE:' のハイライトがいつの間にか消える
    vim.api.nvim_set_hl(
      0,
      'HighlightFixme',
      { ctermbg = 'Red', ctermfg = 'White', bg = '#EBA0AC', fg = '#1E1E2E', bold = true }
    )
    vim.fn.matchadd('HighlightFixme', [[\<FIXME\>:]])

    vim.api.nvim_set_hl(
      0,
      'HighlightTodo',
      { ctermbg = 'Yellow', ctermfg = 'Black', bg = '#F9E2AF', fg = '#1E1E2E', bold = true }
    )
    vim.fn.matchadd('HighlightTodo', [[\<TODO\>:]])

    vim.api.nvim_set_hl(
      0,
      'HighlightNote',
      { ctermbg = 'Cyan', ctermfg = 'Black', bg = '#94E2D5', fg = '#1E1E2E', bold = true }
    )
    vim.fn.matchadd('HighlightNote', [[\<NOTE\>:]])
  end,
})

-- Highlight for FileTypes
vim.api.nvim_create_autocmd('FileType', {
  group = augroup,
  callback = function()
    local m = vim.fn.matchadd('TrailingSpace', [[\s\+$]])

    local excluded_filetypes = { 'help', 'terminal-shell', 'toggleterm', 'git-log', 'git-show', 'gin-log' }
    if vim.tbl_contains(excluded_filetypes, vim.bo.filetype) then
      vim.fn.matchdelete(m)
    end
  end,
})

-- }}}
-- Show CursorLine only on the current window {{{

vim.api.nvim_create_autocmd({ 'BufEnter', 'WinEnter' }, {
  group = augroup,
  callback = function()
    vim.opt.cursorline = true
    vim.opt.cursorcolumn = true
  end,
})

vim.api.nvim_create_autocmd({ 'BufLeave', 'WinLeave' }, {
  group = augroup,
  callback = function()
    vim.opt.cursorline = false
    vim.opt.cursorcolumn = false
  end,
})

-- }}}
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

-- TODO: redraw on winopenpost

-- Custom safe modeline parser (modeline is disabled; only InitLua.allowed_modeline_options are applied) {{{

-- Maps modeline abbreviations to their canonical option names
local modeline_aliases = {
  ft = 'filetype',
  filetype = 'filetype',
  fdm = 'foldmethod',
  foldmethod = 'foldmethod',
}

vim.api.nvim_create_autocmd('BufReadPost', {
  group = augroup,
  callback = function()
    -- TODO: Refactor

    local buf = vim.api.nvim_get_current_buf()
    local line_count = vim.api.nvim_buf_line_count(buf)

    -- Check first and last 5 lines, like Vim's built-in modeline behavior
    local indices = {}
    for i = 0, math.min(4, line_count - 1) do
      table.insert(indices, i)
    end
    for i = math.max(5, line_count - 5), line_count - 1 do
      table.insert(indices, i)
    end

    for _, i in ipairs(indices) do
      local line = vim.api.nvim_buf_get_lines(buf, i, i + 1, false)[1] or ''
      -- Match standard vim modeline: "vim: [set] key=value ..."
      local modeline = line:match('[vV]im:%s*(.+)')
      if modeline then
        modeline = modeline:gsub(':$', ''):gsub('^set%s+', '')
        for key, value in modeline:gmatch('(%w+)=(%S+)') do
          local canonical = modeline_aliases[key]
          if canonical and vim.tbl_contains(InitLua.allowed_modeline_options, canonical) then
            vim.schedule(function()
              pcall(function()
                vim.opt_local[canonical] = value
              end)
            end)
          end
        end
      end
    end
  end,
})

-- }}}

return M
