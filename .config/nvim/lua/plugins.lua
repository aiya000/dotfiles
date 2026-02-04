---プラグイン設定

local fn = require('utils.functions')
local list = require('utils.list')
local nvim = require('nvim')

return {
  -- catppuccin {{{

  {
    'catppuccin/nvim',
    name = 'catppuccin',
    priority = 1000,
    config = function()
      require('catppuccin').setup({
        flavour = 'mocha',
        background = {
          light = 'latte',
          dark = 'mocha',
        },
        transparent_background = false,
        show_end_of_buffer = false,
        term_colors = false,
        dim_inactive = {
          enabled = false,
          shade = 'dark',
          percentage = 0.15,
        },
        no_italic = false,
        no_bold = false,
        styles = {
          comments = { 'italic' },
          conditionals = { 'italic' },
          loops = {},
          functions = {},
          keywords = {},
          strings = {},
          variables = {},
          numbers = {},
          booleans = {},
          properties = {},
          types = {},
          operators = {},
        },
        color_overrides = {},
        custom_highlights = {},
        integrations = {
          cmp = true,
          gitsigns = true,
          nvimtree = true,
          telescope = true,
          notify = false,
          mini = false,
        },
      })

      vim.cmd.colorscheme('catppuccin')
    end,
  },

  -- }}}
  -- telescope {{{

  {
    'nvim-telescope/telescope.nvim',
    dependencies = {
      'nvim-lua/plenary.nvim',
      'nvim-telescope/telescope-fzf-native.nvim',
      'nvim-telescope/telescope-github.nvim',
      'gbprod/yanky.nvim',
      'crispgm/telescope-heading.nvim',
    },
    branch = '0.1.x',
    config = function()
      local actions = require('telescope.actions')
      require('telescope').setup({
        defaults = {
          mappings = {
            n = {
              ['<C-l>'] = actions.close,
              ['<C-j>'] = actions.select_default,
            },
            i = {
              ['<C-l>'] = function()
                nvim.run_with_virtual_keymaps('<Esc>') -- Normal Mode
              end,
              -- ↓ Bash like keys
              ['<C-j>'] = actions.select_default,
              ['<Esc>'] = function()
                nvim.run_with_virtual_keymaps('<Esc>') -- Normal Mode
              end,
              ['<C-b>'] = function()
                nvim.run_with_virtual_keymaps('<Left>')
              end,
              ['<C-f>'] = function()
                nvim.run_with_virtual_keymaps('<Right>')
              end,
              ['<C-a>'] = function()
                nvim.run_with_virtual_keymaps('<Home>')
              end,
              ['<C-e>'] = function()
                nvim.run_with_virtual_keymaps('<End>')
              end,
              ['<C-h>'] = function()
                nvim.run_with_virtual_keymaps('<Backspace>')
              end,
              ['<C-d>'] = function()
                nvim.run_with_virtual_keymaps('<Delete>')
              end,
              -- TODO: 動くように直す
              -- ['<C-u>'] = function()
              --   local current_line = vim.api.nvim_get_current_line()
              --   local cursor_pos = vim.api.nvim_win_get_cursor(0)
              --   local row, col = cursor_pos[1], cursor_pos[2]
              --   vim.api.nvim_set_current_line(current_line:sub(col + 1))
              --   vim.api.nvim_win_set_cursor(0, { row, 0 })
              -- end,
              -- ['<C-k>'] = function()
              --   local current_line = vim.api.nvim_get_current_line()
              --   local cursor_pos = vim.api.nvim_win_get_cursor(0)
              --   local row, col = cursor_pos[1], cursor_pos[2]
              --   vim.api.nvim_set_current_line(current_line:sub(1, col))
              --   vim.api.nvim_win_set_cursor(0, { row, col })
              -- end,
            },
          },
        },
        extensions = {
          file_browser = {
            hijack_netrw = true,
            mappings = {
              i = {},
              n = {
                o = actions.select_default,
                O = actions.select_tab,
                V = actions.select_vertical,
                S = actions.select_horizontal,
              },
            },
          },
          frecency = {
            db_safe_mode = false, -- Disable "remove n entries from database?" dialog
          },
        },
      })
      require('telescope').load_extension('fzf')
      require('telescope').load_extension('gh')
      require('telescope').load_extension('yank_history')
      require('telescope').load_extension('heading')
    end,
  },

  -- }}}
  -- nvim-hlslens {{{

  {
    'kevinhwang91/nvim-hlslens',
    config = function()
      require('hlslens').setup({
        calm_down = true,
        nearest_only = true,
        override_lens = function(render, posList, nearest, idx, relIdx)
          local sfw = vim.v.searchforward == 1
          local indicator, text, chunks
          local absRelIdx = math.abs(relIdx)
          if absRelIdx > 1 then
            indicator = ('%d%s'):format(absRelIdx, sfw ~= (relIdx > 1) and '▲' or '▼')
          elseif absRelIdx == 1 then
            indicator = sfw ~= (relIdx == 1) and '▲' or '▼'
          else
            indicator = ''
          end

          local lnum, col = unpack(posList[idx])
          if nearest then
            local cnt = #posList
            if indicator ~= '' then
              text = ('[%s %d/%d]'):format(indicator, idx, cnt)
            else
              text = ('[%d/%d]'):format(idx, cnt)
            end
            chunks = { { ' ', 'Ignore' }, { text, 'HlSearchLensNear' } }
          else
            text = ('[%s %d]'):format(indicator, idx)
            chunks = { { ' ', 'Ignore' }, { text, 'HlSearchLens' } }
          end
          render.setVirt(0, lnum - 1, col - 1, chunks, nearest)
        end,
      })
    end,
  },

  -- }}}
  -- nvim-highlight-colors {{{

  {
    'brenoprata10/nvim-highlight-colors',
    config = function()
      require('nvim-highlight-colors').setup({
        render = 'background', -- or 'foreground' or 'first_column'
        enable_named_colors = true,
        enable_tailwind = false, -- Enable tailwind colors
      })
    end,
  },
  -- }}}
  -- mason.nvim {{{

  {
    'williamboman/mason.nvim',
    config = function()
      require('mason').setup({
        ui = {
          border = 'rounded',
        },
      })
    end,
  },

  -- }}}
  -- telescope-fzf-native {{{

  {
    'nvim-telescope/telescope-fzf-native.nvim',
    build = 'make',
    cond = function()
      return vim.fn.executable('make') == 1
    end,
  },

  -- }}}
  -- nvim-notify {{{

  {
    'rcarriga/nvim-notify',
    config = function()
      local notify = require('notify')
      notify.setup({
        background_colour = '#000000',
        level = 2,
        render = 'default',
        timeout = 1000000, -- TODO: falseにするとエラーが出るので、いったん大きいテキトーな値に設定
      })
      vim.notify = notify
    end,
  },
  -- }}}
  -- hlchunk.nvim {{{

  {
    'shellRaining/hlchunk.nvim',
    event = { 'BufReadPre', 'BufNewFile' },
    config = function()
      require('hlchunk').setup({
        chunk = { -- NOTE: May need to `TSInstall {filetype}` to show chunk
          enable = true,
          style = '#c678dd',
        },
        indent = {
          enable = true,
        },
        line_num = {
          enable = true,
        },
        blank = {
          -- chunk・indentバッティングするから無効化…
          enable = false,
        },
      })
    end,
  },

  -- }}}
  -- galaxyline {{{

  {
    'NTBBloodbath/galaxyline.nvim',
    config = function()
      local gl = require('galaxyline')
      local condition = require('galaxyline.condition')
      local gls = gl.section
      gl.short_line_list = { 'NvimTree', 'vista', 'dbui', 'packer' }

      local colors = {
        bg = '#282c34',
        fg = '#aab2bf',
        yellow = '#fabd2f',
        cyan = '#008080',
        darkblue = '#081633',
        green = '#afd700',
        orange = '#FF8800',
        violet = '#a9a1e1',
        magenta = '#c678dd',
        blue = '#51afef',
        red = '#ec5f67',
        white = '#ffffff',
      }

      local mode_colors = {
        n = colors.blue,
        i = colors.magenta,
        R = colors.magenta,
        Rv = colors.magenta,
        v = colors.yellow,
        V = colors.yellow,
        s = colors.yellow,
        S = colors.yellow,
        c = colors.green,
        t = colors.yellow,
        ['!'] = colors.red,
      }

      local function get_current_mode_color()
        local color = mode_colors[vim.fn.mode()]
        return color == nil and colors.white or color
      end

      gls.left[1] = {
        CurrentWindowHighlightLeft = {
          provider = fn.const('▊ '),
          highlight = { colors.blue, colors.bg },
        },
      }

      gls.left[2] = {
        ViMode = {
          provider = function()
            -- ハイライトを動的に設定
            vim.api.nvim_set_hl(0, 'GalaxyViMode', {
              fg = get_current_mode_color(),
              bg = colors.bg,
              bold = true,
            })

            return ' '
          end,
          highlight = 'GalaxyViMode',
        },
      }

      gls.left[4] = {
        LineInfo = {
          provider = 'LineColumn',
          separator = ' ',
          separator_highlight = { 'NONE', colors.bg },
          highlight = { colors.fg, colors.bg },
        },
      }

      gls.left[5] = {
        PerCent = {
          provider = 'LinePercent',
          separator = ' ',
          separator_highlight = { 'NONE', colors.bg },
          highlight = { colors.fg, colors.bg, 'bold' },
        },
      }

      -- gls.mid[1] = {
      -- }

      gls.right[1] = {
        ShowLspClient = {
          provider = function()
            local clients = vim.lsp.get_clients({ bufnr = 0 })
            if #clients == 0 then
              return ''
            end
            local client_names = {}
            for _, client in ipairs(clients) do
              table.insert(client_names, client.name)
            end
            return table.concat(client_names, ', ')
          end,
          condition = function()
            return not list.has({ 'dashboard', '' }, vim.bo.filetype)
          end,
          icon = 'LSP: ',
          highlight = { colors.cyan, colors.bg, 'bold' },
        },
      }

      gls.right[2] = {
        FileEncode = {
          provider = 'FileEncode',
          condition = condition.hide_in_width,
          separator = ' ',
          separator_highlight = { 'NONE', colors.bg },
          highlight = { colors.green, colors.bg, 'bold' },
        },
      }

      gls.right[3] = {
        FileFormat = {
          provider = 'FileFormat',
          condition = condition.hide_in_width,
          separator = ' ',
          separator_highlight = { 'NONE', colors.bg },
          highlight = { colors.green, colors.bg, 'bold' },
        },
      }

      gls.right[4] = {
        GitIcon = {
          provider = function()
            return '  '
          end,
          condition = condition.check_git_workspace,
          separator = ' ',
          separator_highlight = { 'NONE', colors.bg },
          highlight = { colors.violet, colors.bg, 'bold' },
        },
      }

      gls.right[5] = {
        GitBranch = {
          provider = 'GitBranch',
          condition = condition.check_git_workspace,
          highlight = { colors.violet, colors.bg, 'bold' },
        },
      }

      gls.right[6] = {
        DiffAdd = {
          provider = 'DiffAdd',
          condition = condition.hide_in_width,
          icon = '  ',
          highlight = { colors.green, colors.bg },
        },
      }

      gls.right[7] = {
        DiffModified = {
          provider = 'DiffModified',
          condition = condition.hide_in_width,
          icon = ' 柳',
          highlight = { colors.orange, colors.bg },
        },
      }

      gls.right[8] = {
        DiffRemove = {
          provider = 'DiffRemove',
          condition = condition.hide_in_width,
          icon = '  ',
          highlight = { colors.red, colors.bg },
        },
      }

      gls.right[9] = {
        RainbowBlue = {
          provider = function()
            return ' ▊'
          end,
          highlight = { colors.blue, colors.bg },
        },
      }

      gls.short_line_left[1] = {
        BufferType = {
          provider = 'FileTypeName',
          separator = ' ',
          separator_highlight = { 'NONE', colors.bg },
          highlight = { colors.blue, colors.bg, 'bold' },
        },
      }

      gls.short_line_left[2] = {
        SFileName = {
          provider = 'SFileName',
          condition = condition.buffer_not_empty,
          highlight = { colors.fg, colors.bg, 'bold' },
        },
      }

      gls.short_line_right[1] = {
        BufferIcon = {
          provider = 'BufferIcon',
          highlight = { colors.fg, colors.bg },
        },
      }
    end,
  },

  -- }}}
  -- bufferline {{{

  {
    'akinsho/bufferline.nvim',
    dependencies = {
      'nvim-tree/nvim-web-devicons',
    },
    config = function()
      require('bufferline').setup({
        options = {
          mode = 'tabs',
          numbers = 'none',
          indicator = {
            icon = '▎',
            style = 'icon',
          },
          buffer_close_icon = '×',
          modified_icon = '●',
          close_icon = '',
          max_name_length = 30,
          diagnostics = 'nvim_lsp',
          color_icons = true,
          show_buffer_icons = true,
          show_buffer_close_icons = true,
          show_close_icon = true,
          separator_style = 'slant',
          always_show_bufferline = true,
        },
      })
    end,
  },

  -- }}}
  -- nvim-web-devicons {{{

  { 'nvim-tree/nvim-web-devicons' },

  -- }}}
  -- jaq-nvim {{{

  {
    'is0n/jaq-nvim',
    opts = {
      cmds = {
        internal = {
          lua = 'luafile %',
          vim = 'source %',
        },
        external = {
          -- markdown = 'glow %', -- 'after/ftplugin/markdown.lua'で管理
          typescript = 'bun run %',
        },
      },

      behavior = {
        default = 'float',
        startinsert = true, -- これやってると、すぐEnterで閉じられる
        wincmd = false,
        autosave = false,
      },

      ui = {
        float = {
          border = { '╔', '═', '╗', '║', '╝', '═', '╚', '║' },
          winhl = 'Normal', -- See ':h winhl'
          borderhl = 'FloatBorder',
          winblend = 0, -- See ':h winblend'

          -- Num from `0-1` for measurements
          height = 0.8,
          width = 0.8,
          x = 0.5,
          y = 0.5,
        },

        terminal = {
          position = 'bot', -- Window position
          size = 10, -- Window size
          line_no = false, -- Disable line numbers
        },

        quickfix = {
          position = 'bot', -- Window position
          size = 10, -- Window size
        },
      },
    },
  },

  -- }}}
  -- vim-neoquickrun {{{

  nvim.load_from_local_or_remote(
    'aiya000/vim-neoquickrun',
    '~/Repository/vim-neoquickrun',
    InitLua.disable_neoquickrun == true,
    {
      init = function() end,
    }
  ),

  -- }}}
  -- vim-submode {{{

  {
    'kana/vim-submode',
    config = function()
      -- Window Resize submode
      vim.fn['submode#enter_with']('winresize', 'n', '', '<C-s>w', '<Nop>')
      vim.fn['submode#map']('winresize', 'n', '', 'j', '<C-w>+')
      vim.fn['submode#map']('winresize', 'n', '', 'k', '<C-w>-')
      vim.fn['submode#map']('winresize', 'n', '', 'h', '3<C-w><')
      vim.fn['submode#map']('winresize', 'n', '', 'l', '3<C-w>>')
      vim.fn['submode#map']('winresize', 'n', '', '=', '<C-w>=')
      vim.fn['submode#map']('winresize', 'n', '', '_', '<C-w>_')
      vim.fn['submode#map']('winresize', 'n', '', '\\|', '<C-w>|')

      -- Tab Move submode
      vim.fn['submode#enter_with']('tabmove', 'n', '', '<C-s>t', '<Nop>')
      vim.keymap.set('n', '<C-s>tn', function()
        vim.fn['submode#enter']('tabmove')
        nvim.move_tab_next()
      end)
      vim.keymap.set('n', '<C-s>tp', function()
        vim.fn['submode#enter']('tabmove')
        nvim.move_tab_prev()
      end)
      vim.fn['submode#map']('tabmove', 'n', 'x', 'n', '<Cmd>lua require("nvim").move_tab_next()<CR>')
      vim.fn['submode#map']('tabmove', 'n', 'x', 'p', '<Cmd>lua require("nvim").move_tab_prev()<CR>')
      vim.fn['submode#map']('tabmove', 'n', '', 'c', '<Cmd>tabnew<CR>')
      vim.fn['submode#map']('tabmove', 'n', '', 'x', '<Cmd>tabclose<CR>')

      -- Window Move submode
      vim.fn['submode#enter_with']('winmove', 'n', '', '<C-s>m', '<Nop>')
      vim.keymap.set('n', '<C-s>mN', function()
        vim.fn['submode#enter']('winmove')
        nvim.move_window_forward()
      end)
      vim.keymap.set('n', '<C-s>mP', function()
        vim.fn['submode#enter']('winmove')
        nvim.move_window_backward()
      end)
      vim.fn['submode#map']('winmove', 'n', 'x', 'N', '<Cmd>lua require("nvim").move_window_forward()<CR>')
      vim.fn['submode#map']('winmove', 'n', 'x', 'P', '<Cmd>lua require("nvim").move_window_backward()<CR>')
      vim.fn['submode#map']('winmove', 'n', '', 'H', '<C-w>H<Cmd>normal! zz<CR>')
      vim.fn['submode#map']('winmove', 'n', '', 'J', '<C-w>J<Cmd>normal! zz<CR>')
      vim.fn['submode#map']('winmove', 'n', '', 'K', '<C-w>K<Cmd>normal! zz<CR>')
      vim.fn['submode#map']('winmove', 'n', '', 'L', '<C-w>L<Cmd>normal! zz<CR>')
      vim.fn['submode#map']('winmove', 'n', '', '_', '<C-w>_')
      vim.fn['submode#map']('winmove', 'n', '', '"', '<Cmd>resize 5<CR>')
      vim.fn['submode#map']('winmove', 'n', '', 'q', '<Nop>')

      -- Yanky Ring submode
      vim.fn['submode#enter_with']('yanky', 'n', '', '<C-s>y', '<Nop>')
      vim.fn['submode#map']('yanky', 'n', '', '<C-p>', '<Plug>(YankyPreviousEntry)')
      vim.fn['submode#map']('yanky', 'n', '', '<C-n>', '<Plug>(YankyNextEntry)')
    end,
  },

  -- }}}
  -- bakaup.vim {{{

  nvim.load_from_local_or_remote('aiya000/bakaup.vim', '~/Repository/bakaup.vim', InitLua.disable_bakaup == true, {
    init = function()
      vim.g.bakaup_auto_backup = 1
      vim.g.bakaup_backup_dir = InitLua.backupdir
    end,
  }),

  -- }}}
  -- plenary.nvim {{{

  {
    'nvim-lua/plenary.nvim',
    lazy = false,
  },

  -- }}}
  -- snacks.nvim {{{

  { 'folke/snacks.nvim' },

  -- }}}
  -- async.vim {{{

  { 'prabirshrestha/async.vim' },

  -- }}}
  -- denops.vim {{{

  {
    'vim-denops/denops.vim',
    lazy = false,
  },

  -- }}}
  -- gin.vim {{{

  {
    'lambdalisue/gin.vim',
    dependencies = { 'vim-denops/denops.vim' },
    config = function()
      vim.g.gin_proxy_editor_opener = 'vsplit'
    end,
  },

  -- }}}
  -- nvim-treesitter {{{

  { 'nvim-treesitter/nvim-treesitter', build = ':TSUpdate' },

  -- }}}
  -- lexima.vim {{{

  {
    'cohama/lexima.vim',
    enabled = not InitLua.recording_mode,
    config = function()
      vim.fn['lexima#add_rule']({ char = '<', input_after = '>' })
      vim.fn['lexima#add_rule']({ char = '「', input_after = '」' })
      vim.fn['lexima#add_rule']({ char = '（', input_after = '）' })
      vim.fn['lexima#add_rule']({ char = '【', input_after = '】' })
    end,
  },

  -- }}}
  -- vim-textobj-user {{{

  { 'kana/vim-textobj-user' },

  -- }}}
  -- vim-textobj-indent {{{

  {
    'kana/vim-textobj-indent',
    dependencies = { 'kana/vim-textobj-user' },
    config = function()
      vim.g.textobj_indent_no_default_key_mappings = 1
    end,
  },

  -- }}}
  -- vim-textobj-from_regexp {{{

  {
    'osyo-manga/vim-textobj-from_regexp',
    dependencies = { 'kana/vim-textobj-user' },
  },

  -- }}}
  -- vim-textobj-xmlattr {{{

  {
    'whatyouhide/vim-textobj-xmlattr',
    dependencies = { 'kana/vim-textobj-user' },
  },

  -- }}}
  -- vim-textobj-jabraces {{{

  {
    'kana/vim-textobj-jabraces',
    dependencies = { 'kana/vim-textobj-user' },
  },

  -- }}}
  -- vim-textobj-between {{{

  {
    'thinca/vim-textobj-between',
    keys = {
      { '<Plug>(textobj-between-a)', mode = { 'v', 'x', 'o' } },
      { '<Plug>(textobj-between-i)', mode = { 'v', 'x', 'o' } },
    },
    config = function()
      vim.g.textobj_between_no_default_key_mappings = 1
    end,
  },

  -- }}}
  -- vim-operator-surround {{{

  -- See `./plugins/vim-operator-surround.lua`

  -- }}}
  -- operator-camelize.vim {{{

  {
    'tyru/operator-camelize.vim',
    keys = {
      { '<Plug>(operator-camelize)', mode = { 'n', 'x' } },
      { '<Plug>(operator-decamelize)', mode = { 'n', 'x' } },
      { '<Plug>(operator-camelize-toggle)', mode = { 'n', 'x' } },
    },
  },

  -- }}}
  -- vim-repeat {{{

  { 'kana/vim-repeat' },

  -- }}}
  -- vim-cursorword {{{

  {
    'itchyny/vim-cursorword',
    config = function()
      vim.api.nvim_set_hl(0, 'CursorWord0', { ctermbg = 'LightGray', ctermfg = 'Black' })
      vim.api.nvim_set_hl(0, 'CursorWord1', { ctermbg = 'LightGray', ctermfg = 'Black' })
    end,
  },

  -- }}}
  -- vim-matchup {{{

  { 'andymass/vim-matchup' },

  -- }}}
  -- flash.nvim {{{

  -- See `./plugins/flash-nvim.lua`

  -- }}}
  -- kensaku.vim {{{

  { 'lambdalisue/kensaku.vim' },

  -- }}}
  -- kensaku-search.vim {{{

  {
    'lambdalisue/kensaku-search.vim',
    dependencies = { 'lambdalisue/kensaku.vim' },
    config = function()
      vim.keymap.set('c', '<CR>', '<Plug>(kensaku-search-replace)<CR>')
    end,
  },

  -- }}}
  -- yanky.nvim {{{

  {
    'gbprod/yanky.nvim',
    enabled = InitLua.disable_yanky ~= true,
    config = function()
      require('yanky').setup({
        ring = {
          history_length = 100,
          storage = 'shada',
          sync_with_numbered_registers = true,
          cancel_event = 'update',
        },
        highlight = {
          on_put = true,
          on_yank = true,
          timer = 200,
        },
        preserve_cursor_position = {
          enabled = true,
        },
      })
    end,
    keys = {
      { 'y', '<Plug>(YankyYank)', mode = { 'n', 'x' } },
      { 'p', '<Plug>(YankyPutAfter)', mode = { 'n', 'v', 'x' } },
      { 'P', '<Plug>(YankyPutBefore)', mode = { 'n', 'v', 'x' } },
      { ']p', '<Plug>(YankyPutIndentAfterLinewise)', mode = 'n' },
      { '[p', '<Plug>(YankyPutIndentBeforeLinewise)', mode = 'n' },
      { ']P', '<Plug>(YankyPutIndentBeforeLinewise)', mode = 'n' },
      { '[P', '<Plug>(YankyPutIndentBeforeLinewise)', mode = 'n' },
      { '>p', '<Plug>(YankyPutIndentAfterShiftRight)', mode = 'n' },
      { '<p', '<Plug>(YankyPutIndentAfterShiftLeft)', mode = 'n' },
      { '>P', '<Plug>(YankyPutIndentBeforeShiftRight)', mode = 'n' },
      { '<P', '<Plug>(YankyPutIndentBeforeShiftLeft)', mode = 'n' },
      { '=p', '<Plug>(YankyPutAfterFilter)', mode = 'n' },
      { '=P', '<Plug>(YankyPutBeforeFilter)', mode = 'n' },

      -- Hydraと連携するために無効化。See './keymaps.lua'
      -- { 'p', '<Plug>(YankyPutAfter)', mode = { 'n', 'x' } },
      -- { 'P', '<Plug>(YankyPutBefore)', mode = { 'n', 'x' } },

      -- Disable because this is overridden by my keymap
      -- { 'gp', '<Plug>(YankyGPutAfter)', mode = { 'n', 'x' } },
      -- { 'gP', '<Plug>(YankyGPutBefore)', mode = { 'n', 'x' } },

      -- See lua/keymaps.lua
      -- { '<C-p>', '<Plug>(YankyPreviousEntry)', mode = 'n' },
      -- { '<C-n>', '<Plug>(YankyNextEntry)', mode = 'n' },
    },
  },

  -- }}}
  -- vim-qfedit {{{

  { 'itchyny/vim-qfedit' },

  -- }}}
  -- quickpeek.vim {{{

  {
    'AndrewRadev/quickpeek.vim',
    ft = 'qf',
    config = function()
      vim.g.quickpeek_auto = true
    end,
  },

  -- }}}
  -- copilot.vim {{{

  {
    'github/copilot.vim',
    enabled = not InitLua.recording_mode,
    config = function()
      vim.g.copilot_no_tab_map = true
    end,
  },

  -- }}}
  -- claudecode.nvim {{{

  -- See `./plugins/claudecode-nvim.lua`

  -- }}}
  -- Align {{{

  { 'vim-scripts/Align' },

  -- }}}
  -- nvim-mado-scratch {{{

  -- See `./plugins/nvim-mado-scratch.lua`

  -- }}}
  -- nvim-just-stay-search {{{

  nvim.load_from_local_or_remote(
    'aiya000/nvim-just-stay-search',
    '~/Repository/nvim-just-stay-search',
    InitLua.disable_just_stay_search == true,
    {
      config = function()
        require('just-stay-search').setup()
      end,
    }
  ),

  -- }}}
  -- vim-write-sync {{{

  {
    'aiya000/vim-write-sync',
    config = function()
      vim.g.write_sync_echo_success_on_write = true
      vim.g.write_sync_lists = {
        { '~/tmp/a', '~/tmp/b', '~/tmp/c' },
        {
          '~/.dotfiles/Windows/Preferences/AutoHotkey.ahk',
          '~/Desktop/AutoHotkey.ahk',
        },
        {
          '~/.dotfiles/Preferences/VSCode/settings.json',
          '~/Windows/AppData/Roaming/Code/User/settings.json',
          '~/Windows/AppData/Roaming/Code - Insiders/User/settings.json',
        },
      }
    end,
  },

  -- }}}
  -- rainbow-delimiters {{{

  {
    'hiphish/rainbow-delimiters.nvim',
    config = function()
      local rainbow_delimiters = require('rainbow-delimiters')

      vim.g.rainbow_delimiters = {
        strategy = {
          [''] = rainbow_delimiters.strategy.global,
          vim = rainbow_delimiters.strategy['local'],
        },
        query = {
          [''] = 'rainbow-delimiters',
          lua = 'rainbow-blocks',
        },
        highlight = {
          'RainbowDelimiterRed',
          'RainbowDelimiterYellow',
          'RainbowDelimiterBlue',
          'RainbowDelimiterOrange',
          'RainbowDelimiterGreen',
          'RainbowDelimiterViolet',
          'RainbowDelimiterCyan',
        },
      }
    end,
  },

  -- }}}
  -- vim-toml {{{

  { 'cespare/vim-toml', ft = 'toml' },

  -- }}}
  -- vim-yaml {{{

  { 'stephpy/vim-yaml', ft = 'yaml' },

  -- }}}
  -- bats.vim {{{

  { 'aliou/bats.vim', ft = 'bats' },

  -- }}}
  -- alex.vim {{{

  { 'vim-scripts/alex.vim', ft = 'alex' },

  -- }}}
  -- vim-gfm-syntax {{{

  { 'rhysd/vim-gfm-syntax', ft = 'markdown' },

  -- }}}
  -- ShaderHighLight {{{

  { 'vim-scripts/ShaderHighLight', ft = 'shaderlab' },

  -- }}}
  -- vim-review {{{

  { 'aiya000/vim-review', ft = 'review' },

  -- }}}
  -- vim-themis {{{

  { 'thinca/vim-themis', ft = { 'vim', 'vimspec' } },

  -- }}}
  -- editorconfig-vim {{{

  { 'editorconfig/editorconfig-vim' },

  -- }}}
  -- fern.vim {{{

  {
    'lambdalisue/fern.vim',

    dependencies = {
      'lambdalisue/vim-fern-git-status',
      'lambdalisue/vim-nerdfont',
      'lambdalisue/vim-fern-renderer-nerdfont',
      'lambdalisue/vim-glyph-palette',
    },

    init = function()
      vim.g['fern#default_hidden'] = 1
      vim.g['fern#drawer_width'] = 40
      vim.g['fern#renderer'] = 'nerdfont'
    end,

    config = function()
      vim.fn['glyph_palette#apply']() -- とてもかっこいい色に設定する
    end,
  },

  -- }}}
  -- ale {{{

  {
    'dense-analysis/ale',
    config = function()
      -- Common
      vim.g.ale_set_highlights = false
      vim.g.ale_vim_vint_show_style_issues = false
      vim.g.ale_virtualtext_cursor = 'current'

      -- Linters
      local function create_hlint_command()
        local ghc_standard_extensions = { -- {{{
          'AutoDeriveTypeable',
          'BangPatterns',
          'BinaryLiterals',
          'ConstraintKinds',
          'DataKinds',
          'DefaultSignatures',
          'DeriveDataTypeable',
          'DeriveFoldable',
          'DeriveFunctor',
          'DeriveGeneric',
          'DeriveTraversable',
          'DoAndIfThenElse',
          'DuplicateRecordFields',
          'EmptyDataDecls',
          'ExistentialQuantification',
          'FlexibleContexts',
          'FlexibleInstances',
          'FunctionalDependencies',
          'GADTs',
          'GeneralizedNewtypeDeriving',
          'InstanceSigs',
          'KindSignatures',
          'LambdaCase',
          'MonadFailDesugaring',
          'MultiParamTypeClasses',
          'MultiWayIf',
          'NamedFieldPuns',
          'NoImplicitPrelude',
          'OverloadedStrings',
          'PartialTypeSignatures',
          'PatternGuards',
          'PolyKinds',
          'RankNTypes',
          'RecordWildCards',
          'ScopedTypeVariables',
          'StandaloneDeriving',
          'TupleSections',
          'TypeApplications',
          'TypeFamilies',
          'TypeSynonymInstances',
          'ViewPatterns',
        } -- }}}
        local extensions = {}
        for _, ext in ipairs(ghc_standard_extensions) do
          table.insert(extensions, '-X ' .. ext)
        end
        return 'hlint ' .. table.concat(extensions, ' ')
      end

      vim.g.ale_linters = {
        haskell = { create_hlint_command(), 'stack ghc' },
        dhall = { 'dhall lint' },
        html = { 'htmlhint', 'tidy' },
        css = { 'csslint', 'stylelint' },
        kotlin = { 'ktlint' },
        java = { 'checkstyle', 'google-java-format', 'PMD' },
      }

      local typescript_variants = {
        'typescript',
        'javascript',
        'vue',
        'typescript.tsx',
        'javascript.jsx',
      }

      for _, ts in ipairs(typescript_variants) do
        vim.g.ale_linters[ts] = { 'prettier', 'eslint' }
      end

      vim.g.ale_scala_scalastyle_config = vim.fn.expand('~/.dotfiles/scalastyle_config_default.xml')

      -- Formatters
      vim.g.ale_fix_on_save = true

      vim.g.ale_fixers = {
        sh = { 'shfmt' },
        go = { 'gofmt', 'goimports' },
      }
      for _, ts in ipairs(typescript_variants) do
        vim.g.ale_fixers[ts] = { 'prettier', 'eslint' }
      end

      local augroup = vim.api.nvim_create_augroup('InitLuaPluginsAle', { clear = true })

      -- Read local tsconfig by deno
      vim.api.nvim_create_autocmd('FileType', {
        group = augroup,
        pattern = { 'typescript', 'javascript' },
        callback = function()
          local local_tsconfig = vim.fn.getcwd() .. '/tsconfig.json'
          if vim.fn.filereadable(local_tsconfig) == 1 then
            vim.g.ale_javascript_deno_lint_options = '--config ' .. local_tsconfig
          end
        end,
      })

      vim.api.nvim_create_autocmd('ColorScheme', {
        group = augroup,
        callback = function()
          vim.api.nvim_set_hl(0, 'ALEError', { ctermbg = 'gray', ctermfg = 'black' })
        end,
      })
    end,
  },

  -- }}}
  -- vim-webpage {{{

  {
    'aiya000/vim-webpage',
    cmd = 'Webpage',
    config = function()
      vim.g.webpage_source = {
        stackage = 'https://www.stackage.org/lts-15.4/hoogle?q=%s',
      }
    end,
  },

  -- }}}
  -- vim-manpager {{{

  { 'lambdalisue/vim-manpager', cmd = 'Man' },

  -- }}}
  -- vim-quickrepl {{{

  {
    'aiya000/vim-quickrepl',
    keys = { '<Plug>(quickrepl-open)' },
    cmd = 'QuickReplOpen',
    config = function()
      vim.g.quickrepl_config = {
        vue = { 'tsx' },
        ['typescript.tsx'] = { 'tsx' },
        go = { 'gore' },
        ps1 = { 'powrshell', 'powershell.exe' },
      }
      vim.g.quickrepl_use_default_key_mapping = true
      vim.g.quickrepl_enable_debug = true
    end,
  },

  -- }}}
  -- asyncrun.vim {{{

  {
    'skywind3000/asyncrun.vim',
    config = function()
      vim.api.nvim_create_autocmd('User', {
        group = vim.api.nvim_create_augroup('InitLuaPluginsAsyncRun', { clear = true }),
        pattern = 'AsyncRunStop',
        callback = function()
          vim.notify(':AsyncRun finished', vim.log.levels.INFO)
        end,
      })
    end,
  },

  -- }}}
  -- adrone.vim {{{

  {
    'aiya000/adrone.vim',
    cmd = { 'AdroneHome', 'AdroneSay', 'AdroneVersion' },
  },

  -- }}}
  -- undotree {{{

  {
    'mbbill/undotree',
    cmd = { 'UndotreeToggle', 'UndotreeFocus', 'UndotreeShow', 'UndotreeHide' },
    init = function()
      vim.keymap.set('n', '<leader>U', '<Cmd>UndotreeToggle<CR>', { silent = true })
    end,
  },

  -- }}}
  -- previm {{{

  {
    'kannokanno/previm',
    ft = 'markdown',
    cmd = 'PrevimOpen',
    config = function()
      vim.g.previm_code_language_show = 1
      vim.g.previm_hard_line_break = 1
      if InitLua.is_wsl then
        vim.g.previm_wsl_mode = true
        vim.g.previm_open_cmd = 'wslview'
      end
    end,
  },

  -- }}}
  -- open-browser.vim {{{

  {
    'tyru/open-browser.vim',
    init = function()
      vim.keymap.set('n', '<leader>w', '<Plug>(openbrowser-open)', { remap = true })
      if InitLua.is_wsl then
        vim.g.openbrowser_browser_commands = {
          { name = 'wslview', args = { '{browser}', '{uri}' } },
        }
      end
    end,
  },

  -- }}}
  -- toggleterm.nvim {{{

  -- TODO: Configure this
  {
    'akinsho/toggleterm.nvim',
    opts = {
      hide_numbers = true,
      shade_filetypes = {},
      shade_terminals = true,
      shading_factor = 2,
      start_in_insert = true,
      insert_mappings = true,
      persist_size = true,
      direction = 'float',
      close_on_exit = true,
      shell = vim.o.shell,
      float_opts = {
        border = 'curved',
        winblend = 0,
        highlights = {
          border = 'Normal',
          background = 'Normal',
        },
      },
    },
  },

  -- }}}
  -- nui.nvim {{{

  { 'MunifTanjim/nui.nvim' },

  -- }}}
  -- cmdpalette.nvim {{{

  -- See `./plugins/cmdpalette-nvim.lua`

  -- }}}
  -- dial.nvim {{{

  {
    'monaqa/dial.nvim',
    config = function()
      vim.keymap.set('n', '<C-a>', function()
        require('dial.map').manipulate('increment', 'normal')
      end)

      vim.keymap.set('n', '<C-x>', function()
        require('dial.map').manipulate('decrement', 'normal')
      end)

      vim.keymap.set('n', 'g<C-a>', function()
        require('dial.map').manipulate('increment', 'gnormal')
      end)

      vim.keymap.set('n', 'g<C-x>', function()
        require('dial.map').manipulate('decrement', 'gnormal')
      end)

      vim.keymap.set('x', '<C-a>', function()
        require('dial.map').manipulate('increment', 'visual')
      end)

      vim.keymap.set('x', '<C-x>', function()
        require('dial.map').manipulate('decrement', 'visual')
      end)

      vim.keymap.set('x', 'g<C-a>', function()
        require('dial.map').manipulate('increment', 'gvisual')
      end)

      vim.keymap.set('x', 'g<C-x>', function()
        require('dial.map').manipulate('decrement', 'gvisual')
      end)

      local augend = require('dial.augend')
      require('dial.config').augends:register_group({
        default = {
          augend.integer.alias.decimal,
          augend.integer.alias.hex,
          augend.date.alias['%Y/%m/%d'],
          augend.date.alias['%Y-%m-%d'],
          augend.constant.alias.bool,
          augend.constant.new({
            elements = { '[ ]', '[x]' },
            word = false,
            cyclic = true,
          }),
          augend.constant.new({
            elements = { '作業中', 'レビュー待ち', 'レビュー対応中', 'マージ済み' },
            word = false,
            cyclic = true,
          }),
          augend.constant.new({
            elements = { 'yes', 'no' },
            word = false,
            cyclic = true,
          }),
        },
      })
    end,
  },

  -- }}}
  -- incline.nvim {{{

  -- See `./plugins/incline-nvim.lua`

  -- }}}
  -- nvim-luasnip-emoji {{{

  nvim.load_from_local_or_remote(
    'aiya000/nvim-luasnip-emoji',
    '~/Repository/nvim-luasnip-emoji',
    InitLua.disable_luasnip_emoji == true,
    {}
  ),

  -- }}}
  -- neoscroll.nvim {{{

  {
    'karb94/neoscroll.nvim',
    config = function()
      local neoscroll = require('neoscroll')

      neoscroll.setup({
        mappings = {
          '<C-b>',
          '<C-f>',
          '<C-u>',
          '<C-d>',
          'zt',
          'zz',
          'zb',
        },
        duration_multiplier = 0.25,
        performance_mode = true,
      })

      local keymaps_opts = {
        duration = 200,
        easing = 'quadratic',
      }

      ---`{line_num}gg` and `{line_num}G` support
      local function goto_line()
        local count = vim.v.count
        local current_line, col = unpack(vim.api.nvim_win_get_cursor(0))
        local distance = count - current_line
        if distance == 0 then
          return
        end

        local distance_to_scroll = distance > 0 and math.min(distance, 100) -- Example: max(200, 100)
          or math.max(distance, -100) -- Example: min(-200, -100)
        neoscroll.scroll(distance_to_scroll, keymaps_opts)
        vim.defer_fn(function()
          vim.api.nvim_win_set_cursor(0, { count, col })
        end, 200)
      end

      vim.keymap.set('n', 'gg', function()
        if vim.v.count ~= 0 then
          goto_line()
          return
        end

        neoscroll.scroll(-100, keymaps_opts)
        vim.defer_fn(function()
          vim.cmd('normal! gg')
        end, 100)
      end)

      vim.keymap.set('n', 'G', function()
        if vim.v.count ~= 0 then
          goto_line()
          return
        end

        neoscroll.scroll(100, keymaps_opts)
        vim.defer_fn(function()
          vim.cmd('normal! G')
        end, 100)
      end)
    end,
  },

  -- }}}
  -- screenkey.nvim {{{

  -- :Screenkey to start
  {
    'NStefan002/screenkey.nvim',
    lazy = false,
    version = '*',
  },

  -- }}}
  -- fidget.nvim {{{

  {
    'j-hui/fidget.nvim',
    opts = {
      -- TODO: 設定する
    },
  },

  -- }}}
  -- nvim-FeMaco.lua {{{

  {
    'AckslD/nvim-FeMaco.lua',
    opts = {
      float_opts = function(_)
        local function get_winsize()
          local ui = vim.api.nvim_list_uis()[1]
          if ui ~= nil then
            return ui.width, ui.height
          else
            return 120, 40
          end
        end

        ---@param width integer --Width of floating window
        ---@param height integer --Height of floating window
        ---@return { width: integer, height: integer, row: integer, col: integer }
        local function create_geometry(width, height)
          local win_width, win_height = get_winsize()
          local row = math.floor((win_height - height) / 2)
          local col = math.floor((win_width - width) / 2)
          return {
            width = width,
            height = height,
            row = row,
            col = col,
          }
        end
        local geometry = create_geometry(100, 40)

        return {
          -- Open at the center of the screen
          width = geometry.width,
          height = geometry.height,
          row = geometry.row,
          col = geometry.col,
          -- Below configurations are copied from `:h FeMaco-femaco-configuration`
          relative = 'cursor',
          anchor = 'NW',
          style = 'minimal',
          border = 'rounded',
          zindex = 1,
        }
      end,

      post_open_float = function(winnr)
        -- Because it disables by default
        vim.opt.number = true
        vim.opt.relativenumber = true

        local bufnr = vim.api.nvim_win_get_buf(winnr)

        vim.keymap.set('n', '<C-l>', '<NOP>', { buffer = bufnr, silent = true })
        vim.keymap.set('n', '<C-l><C-l>', '<Cmd>wq<CR>', { buffer = bufnr, silent = true })
        vim.keymap.set('n', '<Esc>', '<Cmd>wq<CR>', { buffer = bufnr, silent = true })
      end,
    },
  },

  -- }}}
  -- colorful-winsep.nvim {{{

  {
    'nvim-zh/colorful-winsep.nvim',
    config = true,
    event = { 'WinLeave' },
  },

  -- }}}
  -- nvim-cmp {{{

  {
    'hrsh7th/nvim-cmp',
    enabled = not InitLua.recording_mode,
    event = { 'InsertEnter', 'CmdlineEnter' },
    dependencies = {
      'hrsh7th/cmp-nvim-lsp',
      'hrsh7th/cmp-buffer',
      'hrsh7th/cmp-path',
      'hrsh7th/cmp-cmdline',
      'L3MON4D3/LuaSnip',
      'saadparwaiz1/cmp_luasnip',
    },
    config = function()
      local cmp = require('cmp')
      local luasnip = require('luasnip')

      local common_mapping = {
        ['<C-n>'] = cmp.mapping.select_next_item(),
        ['<C-p>'] = cmp.mapping.select_prev_item(),
        ['<C-i>'] = cmp.mapping.select_next_item(),
        ['<Tab>'] = cmp.mapping.select_next_item(),
        ['<CR>'] = cmp.mapping.confirm({ select = false }),
      }

      cmp.setup({
        snippet = {
          expand = function(args)
            luasnip.lsp_expand(args.body)
          end,
        },
        mapping = cmp.mapping.preset.insert(common_mapping),
        sources = cmp.config.sources({
          { name = 'nvim_lsp' },
          { name = 'luasnip' },
        }, {
          { name = 'buffer' },
          { name = 'path' },
        }),
      })

      cmp.setup.cmdline({ '/', '?' }, {
        mapping = cmp.mapping.preset.cmdline(),
        sources = {
          { name = 'buffer' },
        },
      })

      cmp.setup.cmdline(':', {
        mapping = cmp.mapping.preset.cmdline(),
        sources = cmp.config.sources({
          { name = 'path' },
        }, {
          { name = 'cmdline' },
        }),
        matching = { disallow_symbol_nonprefix_matching = false },
      })

      if not InitLua.recording_mode then
        -- See 'cmdpalette.nvim' section for other settings of cmdpalette
        cmp.setup.filetype('cmdpalette', {
          mapping = cmp.mapping.preset.insert(common_mapping),
          sources = cmp.config.sources({
            { name = 'cmdline' },
            { name = 'path' },
          }, {
            { name = 'buffer' },
            { name = 'luasnip' },
          }),
        })
      end
    end,
  },

  -- }}}
  -- LuaSnip {{{

  {
    'L3MON4D3/LuaSnip',
    version = 'v2.*',
    build = 'make install_jsregexp',
    config = function()
      local ls = require('luasnip')
      local types = require('luasnip.util.types')

      -- スニペット展開時に、ジャンプ先にマークを表示する
      -- - (なし): 現在いるノード
      -- - ○: 以前にジャンプした/これからジャンプする先のノード
      -- - ◀: 終端ノード
      -- また、終端に到達したらマークを消す
      ls.config.set_config({
        history = true,
        updateevents = 'TextChanged,TextChangedI',
        ext_opts = {
          [types.insertNode] = {
            active = {
              virt_text = { { '', 'LuasnipInsertNodeActive' } },
              virt_text_pos = 'inline',
              hl_mode = 'combine',
            },
            passive = {
              virt_text = { { '○', 'LuasnipInsertNodePassive' } },
              virt_text_pos = 'inline',
              hl_group = 'LuasnipInsertNodePassive',
              hl_mode = 'combine',
            },
          },
          [types.exitNode] = {
            active = {
              virt_text = { { '', 'LuasnipInsertNodePassive' } },
              virt_text_pos = 'inline',
              hl_mode = 'combine',
            },
            passive = {
              virt_text = { { '◀', 'LuasnipInsertNodePassive' } },
              virt_text_pos = 'inline',
              hl_mode = 'combine',
            },
          },
        },
      })

      nvim.load_luasnips()
    end,
  },

  -- }}}
  -- oil.nvim {{{

  {
    'stevearc/oil.nvim',
    lazy = false,
    config = function()
      require('oil').setup({
        default_file_explorer = true,
        delete_to_trash = true,
        keymaps = {
          Q = { '<Cmd>quit<CR>', mode = 'n' },
          H = { '-', mode = 'n', remap = true }, -- Go to parent directory
        },
        view_options = {
          show_hidden = true,
        },
      })
    end,
  },

  -- }}}
  -- render-markdown.nvim {{{

  ---@module 'render-markdown'
  {
    'MeanderingProgrammer/render-markdown.nvim',
    dependencies = {
      'nvim-treesitter/nvim-treesitter',
      'nvim-tree/nvim-web-devicons',
    },
    opts = {}, ---@type render.md.UserConfig
    keys = {
      {
        '<C-h>r',
        '<Cmd>RenderMarkdown toggle<CR>',
        desc = 'Toggle Markdown Rendering',
        ft = 'markdown',
      },
    },
  },

  -- }}}
  -- telescope-heading.nvim {{{

  {
    'crispgm/telescope-heading.nvim',
    config = function()
      local augroup = vim.api.nvim_create_augroup('InitLuaPluginsTelescopeHeading', { clear = true })

      vim.api.nvim_create_autocmd('FileType', {
        group = augroup,
        pattern = { 'markdown', 'help', 'asciidoc' },
        callback = function()
          -- Override `'<C-k><C-f>'` keymap in './keymaps.lua'
          vim.keymap.set('n', '<C-k><C-f>', function()
            require('telescope').extensions.heading.heading({
              sorting_strategy = 'ascending',
              layout_config = { prompt_position = 'bottom' },
            })
          end, { buffer = true, silent = true })
        end,
      })
    end,
  },

  -- }}}
  -- deepl.nvim {{{

  {
    'walkersumida/deepl.nvim',
    config = function()
      require('deepl').setup()
      vim.keymap.set('n', '<leader>k', 'viw:DeepL JA<CR>')
      vim.keymap.set('n', '<leader>K', 'viw:DeepL EN<CR>')
      vim.keymap.set('v', '<leader>k', ':DeepL JA<CR>')
      vim.keymap.set('v', '<leader>K', ':DeepL EN<CR>')
    end,
  },

  -- }}}
  -- telescope-frecency.nvim {{{

  {
    'nvim-telescope/telescope-frecency.nvim',
    dependencies = { 'telescope.nvim' },
    version = '*',
    config = function()
      require('telescope').load_extension('frecency')
    end,
  },

  -- }}}
}

-- vim: set foldmethod=marker foldlevel=1:
