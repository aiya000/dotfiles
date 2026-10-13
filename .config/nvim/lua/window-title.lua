-- window-local なタイトルを winbar の左端に表示する機能
--
-- incline.nvim のバーはウィンドウ右上に「内容の幅ぶんだけ」浮く作りなので、
-- 1つのバーの中で「左にタイトル・右にファイル名」と離して置くことができない。
-- そこで、ファイル名は従来どおり incline（右）に任せ、
-- タイトルは Neovim 標準の winbar（左端）に出して住み分ける。
--
-- :SetWindowTitle <text> / :UnsetWindowTitle で `w:window_title` を設定・解除する。

local M = {}

-- タイトルを保持する window-local 変数の名前（`w:window_title`）
local WINDOW_TITLE_VAR = 'window_title'

-- incline.nvim の overlap 用に確保していた、タイトル未設定時の winbar 文字列。
-- See `.config/nvim/init.lua` の `vim.opt.winbar`。
local EMPTY_WINBAR = ' '

-- winbar 用のハイライトグループ名
local HL_ICON = 'WindowTitleIcon'
local HL_TEXT = 'WindowTitleText'

-- タイトルブロック先頭に出すアイコン（nerd font: nf-fa-tag, U+F02B）。
-- ソースに生グリフを直書きするとエンコーディング都合で欠落しうるので、
-- コードポイントから UTF-8 文字列を生成する。
local ICON = vim.fn.nr2char(0xf02b)

---Defines the highlight groups used in the winbar (light cyan tone).
local function define_highlights()
  -- incline 側のファイル名ブロックと色調をそろえる
  vim.api.nvim_set_hl(0, HL_ICON, { fg = '#000000', bg = '#5fafd7', default = true })
  vim.api.nvim_set_hl(0, HL_TEXT, { fg = '#000000', bg = '#afd7ff', bold = true, default = true })
end

---Reads the window-local title (`w:window_title`) of `win`.
---Returns nil when it is unset or empty.
---@param win integer -- window id
---@return string | nil
function M.get(win)
  local ok, value = pcall(vim.api.nvim_win_get_var, win, WINDOW_TITLE_VAR)
  if not ok then
    return nil
  end
  if type(value) ~= 'string' or value == '' then
    return nil
  end
  return value
end

---Returns the width of the number/sign/fold columns of `win` (i.e. the offset
---from the window's left edge to where the buffer text starts).
---@param win integer -- window id
---@return integer
local function get_textoff(win)
  local infos = vim.fn.getwininfo(win)
  if infos[1] == nil then
    return 0
  end
  return infos[1].textoff or 0
end

---Builds the winbar string for the window currently being evaluated.
---Returns a left-aligned title block (preceded by a `LineNr`-colored spacer
---that lines up with the buffer's number column) when a title is set,
---otherwise the overlap spacer.
---Called from `vim.opt.winbar` via `v:lua`, so it must be reachable globally.
---@return string
function M.winbar()
  -- winbar 式は各ウィンドウで評価される。評価中のウィンドウIDは g:statusline_winid。
  local win = vim.g.statusline_winid
  if win == nil or win == 0 then
    win = vim.api.nvim_get_current_win()
  end

  local title = M.get(win)
  if title == nil then
    return EMPTY_WINBAR
  end

  -- `%#Group#` でハイライト、`%*` でリセット。`%%` はリテラルの `%`。
  -- ユーザー入力の `%` は winbar 式で解釈されないようエスケープする。
  local safe = title:gsub('%%', '%%%%')

  -- 行番号カラム（＋サイン／fold）の幅ぶん、LineNr 背景のスペースを前置して、
  -- タイトルがちょうど本文の開始位置から始まるようにそろえる。
  local offset = get_textoff(win)
  local spacer = offset > 0 and ('%#LineNr#' .. string.rep(' ', offset) .. '%*') or ''

  return table.concat({
    spacer,
    '%#' .. HL_ICON .. '# ' .. ICON .. ' %*',
    -- タイトル（cyan ブロック）の左右パディングを2つずつにして、少しゆったり見せる
    '%#'
      .. HL_TEXT
      .. '#  '
      .. safe
      .. '  %*',
  })
end

---Sets the window-local title and redraws the winbar.
---@param win integer -- window id
---@param title string
local function set(win, title)
  vim.api.nvim_win_set_var(win, WINDOW_TITLE_VAR, title)
  vim.cmd('redrawstatus!')
end

---Clears the window-local title and redraws the winbar.
---@param win integer -- window id
local function unset(win)
  if pcall(vim.api.nvim_win_get_var, win, WINDOW_TITLE_VAR) then
    pcall(vim.api.nvim_win_del_var, win, WINDOW_TITLE_VAR)
  end
  vim.cmd('redrawstatus!')
end

---Sets up highlights, the winbar expression, and the user commands.
function M.setup()
  define_highlights()

  -- カラースキーム変更後もハイライトを維持する
  vim.api.nvim_create_autocmd('ColorScheme', {
    group = vim.api.nvim_create_augroup('WindowTitleHighlight', { clear = true }),
    callback = define_highlights,
  })

  -- winbar は各ウィンドウで評価される。タイトルがあれば左端に出し、無ければ空白
  vim.opt.winbar = "%{%v:lua.require'window-title'.winbar()%}"

  vim.api.nvim_create_user_command('SetWindowTitle', function(opts)
    local title = vim.trim(opts.args)
    if title == '' then
      vim.notify('SetWindowTitle: title is required', vim.log.levels.ERROR)
      return
    end
    set(vim.api.nvim_get_current_win(), title)
  end, {
    nargs = '+',
    desc = 'Set the window-local title shown on the left of the winbar (w:' .. WINDOW_TITLE_VAR .. ')',
  })

  vim.api.nvim_create_user_command('UnsetWindowTitle', function()
    unset(vim.api.nvim_get_current_win())
  end, {
    nargs = 0,
    desc = 'Clear the window-local title shown on the winbar',
  })
end

return M
