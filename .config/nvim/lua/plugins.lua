---プラグイン設定

local helper = require('helper')
local fn = require('utils.functions')
local s = fn.s

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

  -- Telescope {{{
  {
    'nvim-telescope/telescope.nvim',
    branch = '0.1.x',
    dependencies = {
      'nvim-lua/plenary.nvim',
      'nvim-telescope/telescope-fzf-native.nvim',
    },
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
              -- Same as Neovim insert mode
              ['<Esc>'] = false,
              ['<C-l>'] = actions.close,
              ['<C-j>'] = actions.select_default,
              -- TODO: Implement these keys
              -- Bash like keys
              -- ['<C-b>'] = custom.move_left,
              -- ['<C-f>'] = custom.move_right,
              -- ['<C-a>'] = custom.move_home,
              -- ['<C-e>'] = custom.move_end,
              -- ['<C-h>'] = custom.backspace,
              -- ['<C-d>'] = custom.delete,
            },
          },
        },
      })
      pcall(require('telescope').load_extension, 'fzf')
    end,
  },

  {
    'nvim-telescope/telescope-fzf-native.nvim',
    build = 'make',
    cond = function()
      return vim.fn.executable 'make' == 1
    end,
  },
  -- }}}

  -- UI {{{
  {
    'rcarriga/nvim-notify',
    config = function()
      local notify = require('notify')
      notify.setup({
        background_colour = "#000000",
        fps = 30,
        icons = {
          DEBUG = "",
          ERROR = "",
          INFO = "",
          TRACE = "✎",
          WARN = ""
        },
        level = 2,
        minimum_width = 50,
        render = "default",
        stages = "fade_in_slide_out",
        timeout = 3000,
        top_down = true
      })
      vim.notify = notify
    end,
  },

  {
    'NTBBloodbath/galaxyline.nvim',
    dependencies = { 'nvim-tree/nvim-web-devicons' },
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
      }

      gls.left[1] = {
        RainbowRed = {
          provider = fn.const('▊ '),
          highlight = { colors.blue, colors.bg },
        },
      }
      gls.left[2] = {
        ViMode = {
          provider = function()
            local mode_color = {
              n = colors.red,
              i = colors.green,
              v = colors.blue,
              V = colors.blue,
              c = colors.magenta,
              no = colors.red,
              s = colors.orange,
              S = colors.orange,
              [''] = colors.orange,
              ic = colors.yellow,
              R = colors.violet,
              Rv = colors.violet,
              cv = colors.red,
              ce = colors.red,
              r = colors.cyan,
              rm = colors.cyan,
              ['r?'] = colors.cyan,
              ['!'] = colors.red,
              t = colors.red,
            }
            local current_mode = vim.fn.mode() or 'n'
            local color = mode_color[current_mode] or colors.red
            vim.api.nvim_command('highlight GalaxyViMode guifg=' .. color)
            return '  '
          end,
          highlight = { colors.red, colors.bg, 'bold' },
        },
      }
      gls.left[3] = {
        FileSize = {
          provider = 'FileSize',
          condition = condition.buffer_not_empty,
          highlight = { colors.fg, colors.bg },
        },
      }
      gls.left[4] = {
        FileName = {
          provider = 'FileName',
          condition = condition.buffer_not_empty,
          highlight = { colors.magenta, colors.bg, 'bold' },
        },
      }

      gls.left[5] = {
        LineInfo = {
          provider = 'LineColumn',
          separator = ' ',
          separator_highlight = { 'NONE', colors.bg },
          highlight = { colors.fg, colors.bg },
        },
      }

      gls.left[6] = {
        PerCent = {
          provider = 'LinePercent',
          separator = ' ',
          separator_highlight = { 'NONE', colors.bg },
          highlight = { colors.fg, colors.bg, 'bold' },
        },
      }

      gls.left[7] = {
        DiagnosticError = {
          provider = 'DiagnosticError',
          icon = '  ',
          highlight = { colors.red, colors.bg },
        },
      }
      gls.left[8] = {
        DiagnosticWarn = {
          provider = 'DiagnosticWarn',
          icon = '  ',
          highlight = { colors.yellow, colors.bg },
        },
      }

      gls.left[9] = {
        DiagnosticHint = {
          provider = 'DiagnosticHint',
          icon = '  ',
          highlight = { colors.cyan, colors.bg },
        },
      }

      gls.left[10] = {
        DiagnosticInfo = {
          provider = 'DiagnosticInfo',
          icon = '  ',
          highlight = { colors.blue, colors.bg },
        },
      }

      gls.mid[1] = {
        ShowLspClient = {
          provider = 'GetLspClient',
          condition = function()
            local tbl = { ['dashboard'] = true, [''] = true }
            if tbl[vim.bo.filetype] then
              return false
            end
            return true
          end,
          icon = ' LSP:',
          highlight = { colors.cyan, colors.bg, 'bold' },
        },
      }

      gls.right[1] = {
        FileEncode = {
          provider = 'FileEncode',
          condition = condition.hide_in_width,
          separator = ' ',
          separator_highlight = { 'NONE', colors.bg },
          highlight = { colors.green, colors.bg, 'bold' },
        },
      }

      gls.right[2] = {
        FileFormat = {
          provider = 'FileFormat',
          condition = condition.hide_in_width,
          separator = ' ',
          separator_highlight = { 'NONE', colors.bg },
          highlight = { colors.green, colors.bg, 'bold' },
        },
      }

      gls.right[3] = {
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

      gls.right[4] = {
        GitBranch = {
          provider = 'GitBranch',
          condition = condition.check_git_workspace,
          highlight = { colors.violet, colors.bg, 'bold' },
        },
      }

      gls.right[5] = {
        DiffAdd = {
          provider = 'DiffAdd',
          condition = condition.hide_in_width,
          icon = '  ',
          highlight = { colors.green, colors.bg },
        },
      }
      gls.right[6] = {
        DiffModified = {
          provider = 'DiffModified',
          condition = condition.hide_in_width,
          icon = ' 柳',
          highlight = { colors.orange, colors.bg },
        },
      }
      gls.right[7] = {
        DiffRemove = {
          provider = 'DiffRemove',
          condition = condition.hide_in_width,
          icon = '  ',
          highlight = { colors.red, colors.bg },
        },
      }

      gls.right[8] = {
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

  {
    'akinsho/bufferline.nvim',
    dependencies = { 'nvim-tree/nvim-web-devicons' },
    config = function()
      require("bufferline").setup {
        options = {
          mode = "tabs",
          numbers = 'none',
          indicator = {
            icon = '▎',
            style = 'icon',
          },
          buffer_close_icon = '×',
          modified_icon = '●',
          close_icon = '',
          max_name_length = 30,
          diagnostics = "nvim_lsp",
          color_icons = true,
          show_buffer_icons = true,
          show_buffer_close_icons = true,
          show_close_icon = true,
          separator_style = "slant",
          always_show_bufferline = true,
        },
      }
    end,
  },

  { 'nvim-tree/nvim-web-devicons' },
  -- }}}

  -- vim-quickrun {{{
  {
    'thinca/vim-quickrun',
    dependencies = { 'Shougo/vimproc.vim' },
    cmd = 'QuickRun',
    keys = {
      { '<Plug>(quickrun)', mode = { 'n', 'v' } },
      { '<Plug>(quickrun-op)', mode = { 'n', 'v' } },
    },
    config = function()
      vim.g.quickrun_no_default_key_mappings = 0

      vim.g.quickrun_config = {
        ['_'] = {
          -- Global Config
        },
        html = {
          command = InitLua.open_on_gui,
          outputter = 'null',
          exec = '%c %s:p',
        },
        typescript = {
          command = 'ts-node',
          exec = { '%c %o %s' },
          cmdopt = '',
          tempfile = '%{tempname()}.ts',
        },
        nico = {
          command = 'nicorun',
        },
        haskell = {
          cmdopt = '--ghc-arg=-fprint-explicit-kinds',
          command = 'stack',
          exec = '%c exec runghc -- %o %s',
          runner = 'vimproc',
        },
        lhaskell = {
          command = 'stack exec runghc',
          exec = { 'grep "^>.*$" %s | sed -r "s/^>//g" > %s:p:r.hs', '%c %o %s:p:r.hs' },
          tempfile = '%{tempname()}.lhs',
          ['hook/sweep/files'] = '%S:p:r.hs',
        },
        dot = {
          runner = 'vimproc',
          exec = {
            'dot -T png %o %s -o %s.png',
            'wslview %s.png',
          },
          ['hook/sweep/files'] = '%S:p:r.png',
          ['outputter/error/success'] = 'message',
        },
        python = {
          command = 'python3',
        },
      }

      if InitLua.is_wsl then
        fn.set_vim_dict_field(vim.g, 'quickrun_config', 'ps1', {
          command = 'powershell.exe',
          exec = { '%c `wslpath -m %s`' },
          tempfile = '%{tempname()}.ps1',
        })
      end
    end,
  },
  -- }}}

  -- foldCC {{{
  {
    'LeafCage/foldCC',
    config = function()
      vim.g.foldCCtext_maxchars = 120
    end,
  },
  -- }}}

  -- vim-submode {{{
  {
    'kana/vim-submode',
    config = function()
      vim.g.submode_timeout = 0

      -- Window resize submode
      vim.fn['submode#enter_with']('win_resize', 'n', '', '<C-s>w')
      vim.fn['submode#map']('win_resize', 'n', '', 'j', '3<C-w>+')
      vim.fn['submode#map']('win_resize', 'n', '', 'k', '3<C-w>-')
      vim.fn['submode#map']('win_resize', 'n', '', 'h', '3<C-w><')
      vim.fn['submode#map']('win_resize', 'n', '', 'l', '3<C-w>>')
      vim.fn['submode#map']('win_resize', 'n', '', '<', '20<C-w><')
      vim.fn['submode#map']('win_resize', 'n', '', '>', '20<C-w>>')

      -- Tab move submode
      vim.fn['submode#enter_with']('tab_move', 'n', 's', '<C-s>n', ':<C-u>call vimrc#move_tab_next()<CR>')
      vim.fn['submode#enter_with']('tab_move', 'n', 's', '<C-s>p', ':<C-u>call vimrc#move_tab_prev()<CR>')
      vim.fn['submode#map']('tab_move', 'n', 's', 'n', ':<C-u>call vimrc#move_tab_next()<CR>')
      vim.fn['submode#map']('tab_move', 'n', 's', 'p', ':<C-u>call vimrc#move_tab_prev()<CR>')

      -- Window move submode
      vim.fn['submode#enter_with']('win_move', 'n', 's', '<C-s>N', ':<C-u>call vimrc#move_window_forward()<CR>')
      vim.fn['submode#enter_with']('win_move', 'n', 's', '<C-s>P', ':<C-u>call vimrc#move_window_backward()<CR>')
      vim.fn['submode#map']('win_move', 'n', 's', 'N', ':<C-u>call vimrc#move_window_forward()<CR>')
      vim.fn['submode#map']('win_move', 'n', 's', 'P', ':<C-u>call vimrc#move_window_backward()<CR>')
      vim.fn['submode#map']('win_move', 'n', 'e', 'H', '"\\<C-w>H" .. (foldlevel(".") > 0 ? "zO" : "") .. "zz"')
      vim.fn['submode#map']('win_move', 'n', 'e', 'J', '"\\<C-w>J" .. (foldlevel(".") > 0 ? "zO" : "") .. "zz"')
      vim.fn['submode#map']('win_move', 'n', 'e', 'K', '"\\<C-w>K" .. (foldlevel(".") > 0 ? "zO" : "") .. "zz"')
      vim.fn['submode#map']('win_move', 'n', 'e', 'L', '"\\<C-w>L" .. (foldlevel(".") > 0 ? "zO" : "") .. "zz"')
      vim.fn['submode#map']('win_move', 'n', 's', '_', '<C-w>_')
      vim.fn['submode#map']('win_move', 'n', 's', '"', ':resize 5<CR>')

      -- Setup operator-surround
      vim.schedule(helper.setup_operator_surround)
    end,
  },
  -- }}}

  -- aho-bakaup.vim {{{
  {
    'aiya000/aho-bakaup.vim',
    config = function()
      vim.g.bakaup_backup_dir = InitLua.backupdir
      vim.g.bakaup_auto_backup = 1
    end,
  },
  -- }}}

  -- neosnippet.vim {{{
  {
    'Shougo/neosnippet.vim',
    config = function()
      vim.g['neosnippet#snippets_directory'] = s('{neovim_home}/neosnippets', { neovim_home = InitLua.neovim_home })
      vim.g['neosnippet#disable_select_select_mappings'] = 1
    end,
  },
  -- }}}

  -- Base libraries and dependencies {{{
  { 'Shougo/vimproc.vim', build = 'make' },
  { 'nvim-lua/plenary.nvim' },
  { 'prabirshrestha/async.vim' },
  -- }}}

  -- Git plugins {{{
  { 'tpope/vim-fugitive' },
  { 'vim-denops/denops.vim', lazy = false },
  {
    'lambdalisue/gin.vim',
    dependencies = { 'vim-denops/denops.vim' },
    config = function()
      vim.g.gin_proxy_editor_opener = 'vsplit'
    end,
  },
  -- }}}

  -- LSP and completion {{{
  {
    'prabirshrestha/vim-lsp',
    dependencies = { 'prabirshrestha/async.vim' },
    config = function()
      vim.g.lsp_async_completion = 1
      vim.g.lsp_diagnostics_enabled = 0
      vim.g.lsp_document_code_action_signs_enabled = 0
      vim.g.lsp_log_file = vim.fn.expand('~/vim-lsp.log')
      vim.g.lsp_log_verbose = 1
    end,
  },
  {
    'mattn/vim-lsp-settings',
    config = function()
      vim.g.lsp_settings = {
        solargraph = { disabled = 1 },
      }
      vim.g.lsp_settings_filetype_vue = { 'typescript-language-server', 'volar-server' }
      vim.g.lsp_settings_filetype_typescript = { 'typescript-language-server', 'deno' }
      vim.g.lsp_settings_filetype_javascript = { 'typescript-language-server', 'deno' }
    end,
  },
  -- {
  --   'rhysd/vim-lsp-ale',
  --   dependencies = { 'prabirshrestha/vim-lsp' },
  -- },
  { 'prabirshrestha/asyncomplete.vim' },
  { 'prabirshrestha/asyncomplete-lsp.vim' },
  -- }}}

  -- Syntax and parsing {{{
  { 'nvim-treesitter/nvim-treesitter', build = ':TSUpdate' },
  -- }}}

  -- Text manipulation and editing {{{
  { 'tpope/vim-surround' },
  {
    'cohama/lexima.vim',
    config = function()
      vim.fn['lexima#add_rule']({ char = '<', input_after = '>' })
      vim.fn['lexima#add_rule']({ char = '「', input_after = '」' })
      vim.fn['lexima#add_rule']({ char = '（', input_after = '）' })
      vim.fn['lexima#add_rule']({ char = '【', input_after = '】' })
    end,
  },
  -- }}}

  -- Text objects and operators {{{
  { 'kana/vim-textobj-user' },
  {
    'kana/vim-textobj-indent',
    dependencies = { 'kana/vim-textobj-user' },
    config = function()
      vim.g.textobj_indent_no_default_key_mappings = 1
    end,
  },
  {
    'osyo-manga/vim-textobj-from_regexp',
    dependencies = { 'kana/vim-textobj-user' },
  },
  {
    'whatyouhide/vim-textobj-xmlattr',
    dependencies = { 'kana/vim-textobj-user' },
  },
  {
    'kana/vim-textobj-jabraces',
    dependencies = { 'kana/vim-textobj-user' },
  },
  {
    'thinca/vim-textobj-between',
    keys = {
      { '<Plug>(textobj-between-a)', mode = { 'x', 'o' } },
      { '<Plug>(textobj-between-i)', mode = { 'x', 'o' } },
    },
    config = function()
      vim.g.textobj_between_no_default_key_mappings = 1
    end,
  },

  { 'kana/vim-operator-user' },
  {
    'rhysd/vim-operator-surround',
    dependencies = { 'kana/vim-operator-user' },
  },
  {
    'tyru/operator-camelize.vim',
    keys = {
      { '<Plug>(operator-camelize)', mode = { 'n', 'x' } },
      { '<Plug>(operator-decamelize)', mode = { 'n', 'x' } },
      { '<Plug>(operator-camelize-toggle)', mode = { 'n', 'x' } },
    },
  },
  -- }}}

  -- Utilities and tools {{{
  { 'kana/vim-repeat' },
  { 'chip/vim-fat-finger' },
  { 'itchyny/vim-cursorword' },
  { 'justinmk/vim-dirvish' },
  { 'andymass/vim-matchup' },
  {
    'nathanaelkane/vim-indent-guides',
    config = function()
      vim.g.indent_guides_enable_on_vim_startup = 1
      vim.g.indent_guides_start_level = 2
      vim.g.indent_guides_default_mapping = 0
      vim.g.indent_guides_guide_size = 1
      vim.g.indent_guides_auto_colors = 0
      vim.g.indent_guides_tab_guides = 0
      vim.g.indent_guides_exclude_filetypes = {
        '', 'adrone_home', 'aref_web', 'gitcommit', 'happy', 'haskell',
        'help', 'man', 'markdown', 'review',
      }
    end,
  },
  -- }}}

  -- Search and navigation {{{
  {
    'haya14busa/incsearch.vim',
    keys = {
      { '<Plug>(incsearch-forward)', mode = 'n' },
      { '<Plug>(incsearch-backward)', mode = 'n' },
      { '<Plug>(incsearch-stay)', mode = 'n' },
      { '<Plug>(incsearch-nohl)', mode = 'n' },
      { '<Plug>(incsearch-nohl0)', mode = 'n' },
      { '<Plug>(incsearch-nohl-n)', mode = 'n' },
      { '<Plug>(incsearch-nohl-N)', mode = 'n' },
      { '<Plug>(incsearch-nohl-*)', mode = 'n' },
      { '<Plug>(incsearch-nohl-#)', mode = 'n' },
      { '<Plug>(incsearch-nohl-g*)', mode = 'n' },
      { '<Plug>(incsearch-nohl-g#)', mode = 'n' },
    },
  },
  {
    'thinca/vim-visualstar',
    config = function()
      vim.g.visualstar_extra_commands = 'zzzv'
    end,
  },
  { 'osyo-manga/vim-anzu' },
  { 'deris/vim-shot-f' },
  {
    'aiya000/vim-fmap',
    cmd = 'FNoreMap',
    keys = {
      { '<Plug>(fmap-forward-f)', mode = 'n' },
      { '<Plug>(fmap-backward-f)', mode = 'n' },
      { '<Plug>(fmap-forward-t)', mode = 'n' },
      { '<Plug>(fmap-backward-t)', mode = 'n' },
    },
    config = function()
      vim.g.fmap_use_default_keymappings = false
      vim.g.fmap_escape_keys = { '', '', '' }
    end,
  },
  { 'lambdalisue/kensaku.vim' },
  { 'lambdalisue/kensaku-search.vim' },
  -- }}}

  -- Documentation and help {{{
  { 'vim-jp/vimdoc-ja' },
  {
    'machakann/vim-highlightedyank',
    config = function()
      vim.g.highlightedyank_highlight_duration = 200
    end,
  },
  {
    'gbprod/yanky.nvim',
    dependencies = { 'nvim-telescope/telescope.nvim' },
    config = function()
      require('yanky').setup({
        ring = {
          history_length = 100,
          storage = "shada",
          sync_with_numbered_registers = true,
          cancel_event = "update",
        },
        picker = {
          telescope = {
            mappings = {
              default = require("yanky.picker").actions.put("p"),
              i = {
                ["<c-g>"] = require("yanky.picker").actions.put("P"),
                ["<c-x>"] = require("yanky.picker").actions.delete(),
                ["<c-r>"] = require("yanky.picker").actions.set_register(),
              },
            },
          },
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
      require('telescope').load_extension('yank_history')
    end,
    keys = {
      { "y", "<Plug>(YankyYank)", mode = { "n", "x" } },
      { "p", "<Plug>(YankyPutAfter)", mode = { "n", "x" } },
      { "P", "<Plug>(YankyPutBefore)", mode = { "n", "x" } },
      { "gp", "<Plug>(YankyGPutAfter)", mode = { "n", "x" } },
      { "gP", "<Plug>(YankyGPutBefore)", mode = { "n", "x" } },
      { "<c-p>", "<Plug>(YankyPreviousEntry)", mode = "n" },
      { "<c-n>", "<Plug>(YankyNextEntry)", mode = "n" },
      { "]p", "<Plug>(YankyPutIndentAfterLinewise)", mode = "n" },
      { "[p", "<Plug>(YankyPutIndentBeforeLinewise)", mode = "n" },
      { "]P", "<Plug>(YankyPutIndentAfterLinewise)", mode = "n" },
      { "[P", "<Plug>(YankyPutIndentBeforeLinewise)", mode = "n" },
      { ">p", "<Plug>(YankyPutIndentAfterShiftRight)", mode = "n" },
      { "<p", "<Plug>(YankyPutIndentAfterShiftLeft)", mode = "n" },
      { ">P", "<Plug>(YankyPutIndentBeforeShiftRight)", mode = "n" },
      { "<P", "<Plug>(YankyPutIndentBeforeShiftLeft)", mode = "n" },
      { "=p", "<Plug>(YankyPutAfterFilter)", mode = "n" },
      { "=P", "<Plug>(YankyPutBeforeFilter)", mode = "n" },
      { "<leader>y", "<cmd>Telescope yank_history<cr>", desc = "Open Yank History" },
    },
  },
  { 'itchyny/vim-qfedit' },
  {
    'AndrewRadev/quickpeek.vim',
    ft = 'qf',
    config = function()
      vim.g.quickpeek_auto = true
    end,
  },
  { 'tyru/caw.vim' },
  -- }}}

  -- Development tools {{{
  {
    'github/copilot.vim',
    config = function()
      vim.g.copilot_no_tab_map = true
    end,
  },
  { 'vim-scripts/Align' },
  {
    'aiya000/vim-scratch-buffer',
    config = function()
      vim.g.scratch_buffer_default_open_method = 'vsp'
      vim.g.scratch_buffer_default_buffer_size = nil
      vim.g.scratch_buffer_use_default_keymappings = false
      vim.g.scratch_buffer_file_pattern = {
        when_file_buffer = vim.fn.expand('~/tmp/scratch-%d'),
      }
    end,
  },
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
  {
    'luochen1990/rainbow',
    config = function()
      vim.g.rainbow_active = 1
    end,
  },
  { 'willothy/flatten.nvim' },
  -- }}}

  -- Language specific plugins {{{
  { 'fatih/vim-go', ft = 'go' },
  { 'pangloss/vim-javascript', ft = 'javascript' },
  { 'leafgarland/typescript-vim', ft = 'typescript' },
  { 'peitalin/vim-jsx-typescript', ft = { 'typescript.tsx', 'javascript.jsx' } },
  { 'ianks/vim-tsx', ft = 'tsx' },
  { 'posva/vim-vue', ft = 'vue' },
  { 'cespare/vim-toml', ft = 'toml' },
  { 'stephpy/vim-yaml', ft = 'yaml' },
  { 'gutenye/json5.vim', ft = 'json5' },
  { 'rust-lang/rust.vim', ft = 'rust' },
  { 'dag/vim-fish', ft = 'fish' },
  { 'PProvost/vim-ps1', ft = 'ps1' },
  { 'aliou/bats.vim', ft = 'bats' },
  { 'vim-crystal/vim-crystal', ft = 'crystal' },
  { 'udalov/kotlin-vim', ft = 'kotlin' },
  { 'derekwyatt/vim-scala', ft = 'scala' },
  { 'neovimhaskell/haskell-vim', ft = 'haskell' },
  { 'vmchale/ghci-syntax', ft = 'dot-ghci' },
  { 'itchyny/vim-haskell-sort-import', ft = 'haskell' },
  { 'aiya000/vim-ghcid-quickfix', ft = { 'haskell', 'happy', 'alex' } },
  { 'thinca/vim-ft-clojure', ft = { 'clojure', 'lisp' } },
  { 'vim-scripts/alex.vim', ft = 'alex' },
  { 'rhysd/vim-gfm-syntax', ft = 'markdown' },
  { 'vmchale/dhall-vim', ft = 'dhall' },
  { 'ap/vim-css-color', ft = { 'css', 'scss', 'sass', 'html', 'xml', 'typescript.tsx' } },
  { 'delphinus/vim-firestore', ft = 'firestore' },
  { 'jparise/vim-graphql', ft = 'graphql' },
  { 'vim-scripts/ShaderHighLight', ft = 'shaderlab' },
  { 'aiya000/vim-review', ft = 'review' },
  { 'thinca/vim-themis', ft = { 'vim', 'vimspec' } },
  -- }}}

  -- Miscellaneous and rarely used {{{
  { 'Shougo/neomru.vim' },
  { 'osyo-manga/vim-textobj-from_regexp', dependencies = { 'kana/vim-textobj-user' } },
  { 'vim-jp/autofmt' },
  { 'editorconfig/editorconfig-vim' },
  { 'Shougo/context_filetype.vim',
    config = function()
      vim.g.context_filetype_filetypes = {
        help = {},
        vue = {},
        html = {},
        erb = {},
        review = {
          {
            start = '//list\\[[^]]\\+\\]\\[[^]]\\+\\]\\[\\([^]]\\+\\)\\]{',
            ['end'] = '//}',
            filetype = '\\1',
          },
        },
      }
    end,
  },
  {
    'osyo-manga/vim-precious',
    config = function()
      vim.g.precious_enable_switch_CursorMoved = {
        ['*'] = false,
      }
      vim.g.precious_enable_switch_CursorMoved_i = vim.g.precious_enable_switch_CursorMoved
      vim.g.precious_enable_switchers = {}
      vim.g.textobj_precious_no_default_key_mappings = true
    end,
  },
  {
    'tyru/sync-term-cwd.vim',
    config = function()
      vim.g.synctermcwd_cd_command = 'lcd'
    end,
  },
  {
    'lambdalisue/fern.vim',
    config = function()
      vim.g.fern_default_hidden = 1
    end,
  },
  -- }}}

  -- ALE (Asynchronous Lint Engine) {{{
  {
    'dense-analysis/ale',
    config = function()
      -- Common
      vim.g.ale_set_highlights = false
      vim.g.ale_vim_vint_show_style_issues = false
      vim.g.ale_virtualtext_cursor = 'current'

      -- Linters
      local ghc_standard_extensions = {
        'AutoDeriveTypeable', 'BangPatterns', 'BinaryLiterals', 'ConstraintKinds',
        'DataKinds', 'DefaultSignatures', 'DeriveDataTypeable', 'DeriveFoldable',
        'DeriveFunctor', 'DeriveGeneric', 'DeriveTraversable', 'DoAndIfThenElse',
        'DuplicateRecordFields', 'EmptyDataDecls', 'ExistentialQuantification',
        'FlexibleContexts', 'FlexibleInstances', 'FunctionalDependencies', 'GADTs',
        'GeneralizedNewtypeDeriving', 'InstanceSigs', 'KindSignatures', 'LambdaCase',
        'MonadFailDesugaring', 'MultiParamTypeClasses', 'MultiWayIf', 'NamedFieldPuns',
        'NoImplicitPrelude', 'OverloadedStrings', 'PartialTypeSignatures', 'PatternGuards',
        'PolyKinds', 'RankNTypes', 'RecordWildCards', 'ScopedTypeVariables',
        'StandaloneDeriving', 'TupleSections', 'TypeApplications', 'TypeFamilies',
        'TypeSynonymInstances', 'ViewPatterns',
      }

      local function create_hlint_command()
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
        'typescript', 'javascript', 'vue', 'typescript.tsx', 'javascript.jsx',
      }

      for _, ts in ipairs(typescript_variants) do
        vim.g.ale_linters[ts] = { 'prettier', 'eslint', 'vim-lsp' }
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
    end,
  },
  -- }}}

  -- External tools and rare plugins {{{
  {
    'skanehira/translate.vim',
    cmd = {
      'Translate',
      'AutoTranslateModeToggle',
      'AutoTranslateModeEnable',
      'AutoTranslateModeDisable',
    },
    config = function()
      vim.g.translate_source = 'en'
      vim.g.translate_target = 'ja'
      vim.g.translate_winsize = 10
    end,
  },
  {
    'ryicoh/deepl.vim',
    config = function()
      vim.g.deepl_endpoint = 'https://api-free.deepl.com/v2/translate'
    end,
  },
  {
    'aiya000/vim-webpage',
    cmd = 'Webpage',
    config = function()
      vim.g.webpage_source = {
        weblio = 'http://ejje.weblio.jp/content/%s',
        stackage = 'https://www.stackage.org/lts-15.4/hoogle?q=%s',
      }
    end,
  },
  { 'segeljakt/vim-silicon', cmd = { 'Silicon', 'SiliconHighlight' } },
  { 'lambdalisue/vim-manpager', cmd = 'Man' },
  { 'mattn/webapi-vim' },
  { 'mattn/vim-gist', cmd = 'Gist', dependencies = { 'mattn/webapi-vim' } },
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
  { 'skywind3000/asyncrun.vim' },
  { 'chikatoike/concealedyank.vim' },
  { 'aiya000/lucariox.vim' },
  {
    'aiya000/adrone.vim',
    cmd = { 'AdroneHome', 'AdroneSay', 'AdroneVersion' },
  },
  {
    'mbbill/undotree',
    cmd = { 'UndotreeToggle', 'UndotreeFocus', 'UndotreeShow', 'UndotreeHide' },
  },
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

  -- Browser integration {{{
  {
    'tyru/open-browser.vim',
    config = function()
      vim.g.openbrowser_browser_commands = {
        { name = 'wslview', args = { '{browser}', '{uri}' } },
      }
    end,
  },
  {
    'tyru/open-browser-github.vim',
    cmd = { 'OpenGithubFile', 'OpenGithubIssue', 'OpenGithubPullReq', 'OpenGithubProject' },
  },
  -- }}}
}
