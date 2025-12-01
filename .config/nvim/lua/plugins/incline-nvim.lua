local list = require('utils.list')

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
  return vim.fn.fnamemodify(bufname, ':p')
    :gsub('^' .. vim.pesc(project_root) .. '/', '')
    :gsub('^%./', '')
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

local function render(props)
  return vim.bo[props.buf].filetype == 'oil'
    and get_oil_current_dir_renderer(props.buf)
    or get_file_renderer(props.buf, props.focused)
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
          -- その他の special バッファタイプは無視
          return buftype == 'nofile'
          or buftype == 'prompt'
          or buftype == 'quickfix'
          or buftype == 'terminal'
        end,
        filetypes = {},
        floating_wins = true,
        unlisted_buffers = false, -- oil.nvimはunlistedなのでfalseにする
        wintypes = 'special',
      },
    })
  end,
}
