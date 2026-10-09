local list = require('utils.list')

-- :SetWindowTitle / :UnsetWindowTitle で設定・解除する window-local 変数の名前
local WINDOW_TITLE_VAR = 'window_title'

---Reads the window-local title (`w:window_title`) of `win`.
---Returns nil when it is unset or empty.
---@param win integer -- window id
---@return string | nil
local function get_window_title(win)
  local ok, value = pcall(vim.api.nvim_win_get_var, win, WINDOW_TITLE_VAR)
  if not ok then
    return nil
  end
  if type(value) ~= 'string' or value == '' then
    return nil
  end
  return value
end

---Builds the incline block that shows the window-local title on the left.
---Returns nil when no title is set, so callers can omit it entirely.
---@param win integer -- window id
---@return table | nil
local function get_window_title_renderer(win)
  local title = get_window_title(win)
  if title == nil then
    return nil
  end

  return {
    { ' ', '', ' ', guibg = '#5fafd7', guifg = '#000000' },
    ' ',
    { title, gui = 'bold', guifg = '#000000' },
    ' ',
    guibg = '#afd7ff',
  }
end

---Sets the window-local title and refreshes incline so the bar updates immediately.
---@param win integer -- window id
---@param title string
local function set_window_title(win, title)
  vim.api.nvim_win_set_var(win, WINDOW_TITLE_VAR, title)
  require('incline').refresh()
end

---Clears the window-local title and refreshes incline.
---@param win integer -- window id
local function unset_window_title(win)
  -- 変数が無い状態でも呼ばれうるので、存在チェックしてから消す
  if get_window_title(win) ~= nil or pcall(vim.api.nvim_win_get_var, win, WINDOW_TITLE_VAR) then
    pcall(vim.api.nvim_win_del_var, win, WINDOW_TITLE_VAR)
  end
  require('incline').refresh()
end

---Registers :SetWindowTitle / :UnsetWindowTitle.
local function register_window_title_commands()
  vim.api.nvim_create_user_command('SetWindowTitle', function(opts)
    local title = vim.trim(opts.args)
    if title == '' then
      vim.notify('SetWindowTitle: title is required', vim.log.levels.ERROR)
      return
    end
    set_window_title(vim.api.nvim_get_current_win(), title)
  end, {
    nargs = '+',
    desc = 'Set the window-local title shown on the incline bar (w:' .. WINDOW_TITLE_VAR .. ')',
  })

  vim.api.nvim_create_user_command('UnsetWindowTitle', function()
    unset_window_title(vim.api.nvim_get_current_win())
  end, {
    nargs = 0,
    desc = 'Clear the window-local title shown on the incline bar',
  })
end

local function get_oil_current_dir_renderer(buf)
  local helpers = require('incline.helpers')
  local oil = require('oil')

  local dir = oil.get_current_dir(buf)
  local display_dir = dir and vim.fn.fnamemodify(dir, ':~')
  local folder_icon = ''
  local icon_color = '#5fafd7'

  return {
    { ' ', folder_icon, ' ', guibg = icon_color, guifg = helpers.contrast_color(icon_color) },
    ' ',
    { display_dir, gui = 'bold' },
    ' ',
    guibg = '#afafff',
    guifg = '#000000',
  }
end

---@param base_dir string
---@return string | nil
local function read_project_root(base_dir)
  local node_root = require('nodejs').read_node_root_dir(base_dir)
  if node_root ~= nil then
    return node_root
  end

  local git_root = require('git').read_git_root()
  if git_root ~= nil then
    return git_root
  end

  return nil
end

---Gets the file name of `buf` relative to the project root
---@param buf integer -- non-negative integer
---@return string
local function get_filename(buf)
  local bufname = vim.api.nvim_buf_get_name(buf)
  if bufname == '' then
    return '[No Name]'
  end

  local file_dir = vim.fn.fnamemodify(bufname, ':h')
  local project_root = read_project_root(file_dir)
  if project_root == nil then
    -- プロジェクトルートが見つからない場合はファイル名のみ
    return vim.fn.fnamemodify(bufname, ':t')
  end

  -- プロジェクトルートからの相対パスを取得
  return vim.fn.fnamemodify(bufname, ':p'):gsub('^' .. vim.pesc(project_root) .. '/', ''):gsub('^%./', '')
end

local function get_file_renderer(buf, focused)
  local helpers = require('incline.helpers')
  local navic = require('nvim-navic')
  local devicons = require('nvim-web-devicons')

  local filename = get_filename(buf)
  local ft_icon, ft_color = devicons.get_icon_color(filename)
  local modified = vim.bo[buf].modified

  return list.concat(
    {
      ft_icon and { ' ', ft_icon, ' ', guibg = ft_color, guifg = helpers.contrast_color(ft_color) } or '',
      ' ',
      { filename, gui = modified and 'bold,italic' or 'bold' },
      guibg = '#afafff',
      guifg = '#000000',
    },
    focused and navic.get_data(buf) or {}, -- パンくずリスト
    { ' ' }
  )
end

--[[
TODO: [1] 以下はかつて「ターミナルバッファにシェル（もしくはカレントプロセス）のカレントディレクトリを表示」しようとしたときに、うまくいかなかったときの、進捗メモ。実装する
Terminal表示機能の実装進捗（Neovimが固まる問題で一時停止）

実装しようとした内容：
- Terminalバッファでカレントディレクトリを表示
- シェルのプロセスIDから実際のカレントディレクトリを取得

試した方法：
1. /proc/{pid}/cwd を使った方法 → macOSでは/procが存在しない
2. lsofコマンドを使った方法 → ブロッキングによりNeovimが固まる
3. 固定文字列表示 → それでも固まる問題が発生

問題の原因：
- macOSでの/proc未対応
- lsofコマンドでのブロッキング
- terminalバッファでのincline表示自体に技術的課題

今後の課題：
- より安全なプロセス情報取得方法の検討
- 非同期処理での実装
- 別の表示手段（ステータスライン等）の検討

実装予定だった関数：
local function get_terminal_cwd(buf)
  -- terminal jobのプロセスIDからカレントディレクトリを取得
end

local function get_terminal_renderer(buf)
  -- 段階1: まずは固定文字列で表示テスト
  return {
    ' 🖥️ Terminal ',
    guibg = '#4CAF50',
    guifg = '#ffffff',
  }
end
--]]

local function render(props)
  local filetype = vim.bo[props.buf].filetype

  local body
  if filetype == 'oil' then
    body = get_oil_current_dir_renderer(props.buf)
  --[[
  -- 'TODO: [1]'を参照
  elseif filetype == 'terminal' then
    body = get_terminal_renderer(props.buf)
  --]]
  else
    body = get_file_renderer(props.buf, props.focused)
  end

  -- w:window_title が設定されているときだけ、ファイル名の左側にタイトルを連結する
  local title = get_window_title_renderer(props.win)
  if title == nil then
    return body
  end

  return { title, ' ', body }
end

return {
  'b0o/incline.nvim',

  dependencies = {
    'SmiteshP/nvim-navic',
    'nvim-tree/nvim-web-devicons',
  },

  config = function()
    require('incline').setup({
      render = render,

      window = {
        padding = 0,
        margin = { horizontal = 0, vertical = 0 },
        overlap = { -- ウィンドウの1行目を入力しているときに、incline.nvimのバーと入力中のカーソルが被らないようにする
          borders = true,
          statusline = false,
          tabline = false,
          winbar = true,
        },
      },

      -- oil.nvimのようなunlisted bufferも表示する
      ignore = {
        buftypes = function(_, buftype)
          -- oil.nvimのacwriteバッファは無視しない
          if buftype == 'acwrite' then
            return false
          end
          -- ~terminalバッファは無視しない（段階的テストのため）~ See 'TODO: [1]'
          if buftype == 'terminal' then
            -- return false
            return true
          end
          -- その他の special バッファタイプは無視
          return buftype == 'nofile' or buftype == 'prompt' or buftype == 'quickfix'
        end,
        filetypes = {},
        floating_wins = true,
        unlisted_buffers = false, -- oil.nvimはunlistedなのでfalseにする
        wintypes = 'special',
      },
    })

    register_window_title_commands()
  end,
}
