---プラグイン設定（ただしキーマッピングは`./keymaps.lua`に書く）

local helper = require('helper')
local autocmds = require('autocmds')
local fn = require('utils.functions')
local s = fn.s

---@param cond boolean --Pull plugin from lazynvim_plugin_table.dir , or from lazynvim_plugin_table[1]
---@param lazynvim_plugin_table table --LazyPlugin
local function load_local_or_upstream(cond, lazynvim_plugin_table)
  if cond then
    lazynvim_plugin_table[1] = nil
  else
    lazynvim_plugin_table.dir = nil
  end
  return lazynvim_plugin_table
end

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
  -- nvim-lspconfig {{{
  {
    'neovim/nvim-lspconfig',
    config = function()
      local lspconfig = require('lspconfig')

      -- ホバーウィンドウの設定を改善
      vim.lsp.handlers['textDocument/hover'] = vim.lsp.with(vim.lsp.handlers.hover, {
        border = 'rounded',
        max_width = 80,
        max_height = 20,
        focusable = true,
      })

      -- 診断表示の設定
      vim.diagnostic.config({
        virtual_text = true, -- 行末に診断テキストを表示
        signs = true, -- サインカラムに表示
        underline = true, -- 下線表示
        update_in_insert = false, -- インサートモード中は更新しない
        severity_sort = true, -- 重要度でソート
        float = {
          border = 'rounded',
          source = 'always',
          header = '',
          prefix = '',
        },
      })

      -- LSPクライアント接続時のキーマッピング設定
      local function on_attach(client, bufnr)
        -- ホバーウィンドウにフォーカスを移すキーマッピング
        vim.keymap.set('n', '<C-g><C-f>', function()
          vim.lsp.buf.hover()
          -- 少し待ってからフローティングウィンドウにフォーカス
          vim.defer_fn(function()
            local wins = vim.api.nvim_list_wins()
            for _, win in ipairs(wins) do
              if vim.api.nvim_win_get_config(win).relative ~= '' then
                vim.api.nvim_set_current_win(win)
                break
              end
            end
          end, 100)
        end, { buffer = bufnr, desc = 'LSP hover with focus' })

        -- カーソル行の診断を自動表示
        vim.api.nvim_create_autocmd('CursorHold', {
          buffer = bufnr,
          callback = function()
            local opts = {
              focusable = false,
              close_events = { 'BufLeave', 'CursorMoved', 'InsertEnter', 'FocusLost' },
              border = 'rounded',
              source = 'always',
              prefix = ' ',
              scope = 'cursor',
            }
            vim.diagnostic.open_float(nil, opts)
          end,
        })
      end

      -- 共通設定
      local capabilities = require('cmp_nvim_lsp').default_capabilities()

      lspconfig.ts_ls.setup({
        capabilities = capabilities,
        on_attach = on_attach,
      })

      lspconfig.lua_ls.setup({
        capabilities = capabilities,
        on_attach = on_attach,
        settings = {
          Lua = {
            runtime = { version = 'LuaJIT' },
            diagnostics = { globals = { 'vim' } },
            workspace = { library = vim.api.nvim_get_runtime_file('', true) },
            telemetry = { enable = false },
          },
        },
      })
    end,
  },
  -- }}}
  -- nvim-cmp {{{
  {
    'hrsh7th/nvim-cmp',
    dependencies = {
      'hrsh7th/cmp-nvim-lsp', -- LSP補完
      'hrsh7th/cmp-buffer', -- バッファ補完
      'hrsh7th/cmp-path', -- パス補完
      'hrsh7th/cmp-cmdline', -- コマンドライン補完
      'L3MON4D3/LuaSnip', -- スニペットエンジン
      'saadparwaiz1/cmp_luasnip', -- LuaSnipとの統合
    },
    config = function()
      local cmp = require('cmp')
      local luasnip = require('luasnip')

      cmp.setup({
        snippet = {
          expand = function(args)
            luasnip.lsp_expand(args.body)
          end,
        },
        mapping = cmp.mapping.preset.insert({
          ['<C-d>'] = cmp.mapping.scroll_docs(-4),
          ['<C-f>'] = cmp.mapping.scroll_docs(4),
          ['<C-Space>'] = cmp.mapping.complete(),
          ['<C-e>'] = cmp.mapping.abort(),
          ['<CR>'] = cmp.mapping.confirm({ select = true }),
          ['<Tab>'] = cmp.mapping(function(fallback)
            if cmp.visible() then
              cmp.select_next_item()
            elseif luasnip.expand_or_jumpable() then
              luasnip.expand_or_jump()
            else
              fallback()
            end
          end, { 'i', 's' }),
          ['<S-Tab>'] = cmp.mapping(function(fallback)
            if cmp.visible() then
              cmp.select_prev_item()
            elseif luasnip.jumpable(-1) then
              luasnip.jump(-1)
            else
              fallback()
            end
          end, { 'i', 's' }),
        }),
        sources = cmp.config.sources({
          { name = 'nvim_lsp' },
          { name = 'luasnip' },
        }, {
          { name = 'buffer' },
          { name = 'path' },
        }),
      })

      -- コマンドライン補完
      cmp.setup.cmdline(':', {
        mapping = cmp.mapping.preset.cmdline(),
        sources = cmp.config.sources({
          { name = 'path' },
        }, {
          { name = 'cmdline' },
        }),
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
  -- mason-lspconfig.nvim {{{
  {
    'williamboman/mason-lspconfig.nvim',
    config = function()
      require('mason-lspconfig').setup({
        ensure_installed = { 'lua_ls', 'ts_ls' },
        automatic_installation = true,
        handlers = {
          -- デフォルトハンドラー（自動セットアップを無効にして重複を防ぐ）
          function(server_name)
            -- 何もしない（nvim-lspconfigで手動設定しているため）
          end,
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
        fps = 30,
        icons = {
          DEBUG = '',
          ERROR = '',
          INFO = '',
          TRACE = '✎',
          WARN = '',
        },
        level = 2,
        minimum_width = 50,
        render = 'default',
        stages = 'fade_in_slide_out',
        timeout = false,
        top_down = true,
      })
      vim.notify = notify
    end,
  },
  -- }}}
  -- indentmini.nvim {{{
  {
    'nvimdev/indentmini.nvim',
    config = function()
      require('indentmini').setup({
        exclude = {
          'help',
          'lazy',
          'mason',
        }
      })
      vim.cmd.highlight('IndentLine guifg=#454545')
      vim.cmd.highlight('IndentLineCurrent guifg=#123456')
    end,
  },
  -- }}}
  -- galaxyline {{{
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
  -- }}}
  -- bufferline {{{
  {
    'akinsho/bufferline.nvim',
    dependencies = { 'nvim-tree/nvim-web-devicons' },
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
  -- hydra.nvim {{{
  {
    'anuvyklack/hydra.nvim',
    config = function()
      local Hydra = require('hydra')
      local helper = require('helper')

      Hydra({
        name = 'Window Resize',
        mode = 'n',
        body = '<C-s>w',
        heads = {
          { 'j', '<C-w>+', { desc = 'increase height' } },
          { 'k', '<C-w>-', { desc = 'decrease height' } },
          { 'h', '3<C-w><', { desc = 'decrease width' } },
          { 'l', '3<C-w>>', { desc = 'increase width' } },
          { '<Esc>', nil, { exit = true, desc = 'exit' } },
        },
        config = {
          hint = {
            type = 'window',
            border = 'rounded',
          },
        },
      })

      InitLua.hydra.tab_move = Hydra({
        name = 'Tab Move',
        mode = 'n',
        body = 'dummy key',
        heads = {
          {
            'n',
            function()
              helper.move_tab_next()
            end,
            { desc = 'next tab' },
          },
          {
            'p',
            function()
              helper.move_tab_prev()
            end,
            { desc = 'prev tab' },
          },
          { '<Esc>', nil, { exit = true, desc = 'exit' } },
        },
        config = {
          hint = {
            type = 'window',
            border = 'rounded',
          },
        },
      })

      InitLua.hydra.window_move = Hydra({
        name = 'Window Move',
        mode = 'n',
        body = 'dummy key',
        heads = {
          {
            'N',
            function()
              helper.move_window_forward()
            end,
            { desc = 'move forward' },
          },
          {
            'P',
            function()
              helper.move_window_backward()
            end,
            { desc = 'move backward' },
          },
          { 'H', '<C-w>H<Cmd>normal! zz<CR>', { desc = 'move left' } },
          { 'J', '<C-w>J<Cmd>normal! zz<CR>', { desc = 'move down' } },
          { 'K', '<C-w>K<Cmd>normal! zz<CR>', { desc = 'move up' } },
          { 'L', '<C-w>L<Cmd>normal! zz<CR>', { desc = 'move right' } },
          { '_', '<C-w>_', { desc = 'maximize height' } },
          { '"', '<Cmd>resize 5<CR>', { desc = 'resize to 5' } },
          { 'q', nil, { exit = true, desc = 'exit' } },
          { '<Esc>', nil, { exit = true, desc = 'exit' } },
        },
        config = {
          timeout = false,
          hint = {
            type = 'window',
            position = 'bottom',
            offset = 0,
            border = 'rounded',
          },
        },
      })
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
  -- plenary.nvim {{{

  { 'nvim-lua/plenary.nvim' },

  -- }}}
  -- async.vim {{{

  { 'prabirshrestha/async.vim' },

  -- }}}
  -- denops.vim {{{

  { 'vim-denops/denops.vim', lazy = false },

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
  -- vim-surround {{{

  { 'tpope/vim-surround' },

  -- }}}
  -- lexima.vim {{{

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
      { '<Plug>(textobj-between-a)', mode = { 'x', 'o' } },
      { '<Plug>(textobj-between-i)', mode = { 'x', 'o' } },
    },
    config = function()
      vim.g.textobj_between_no_default_key_mappings = 1
    end,
  },

  -- }}}
  -- vim-operator-user {{{

  { 'kana/vim-operator-user' },

  -- }}}
  -- vim-operator-surround {{{

  {
    'rhysd/vim-operator-surround',
    dependencies = { 'kana/vim-operator-user' },
    config = function()
      vim.schedule(helper.setup_operator_surround)
    end,
  },

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

  { 'itchyny/vim-cursorword' },

  -- }}}
  -- vim-dirvish {{{

  { 'justinmk/vim-dirvish' },

  -- }}}
  -- vim-matchup {{{

  { 'andymass/vim-matchup' },

  -- }}}
  -- vim-shot-f {{{

  { 'deris/vim-shot-f' },

  -- }}}
  -- vim-fmap {{{

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

  -- }}}
  -- kensaku.vim {{{

  { 'lambdalisue/kensaku.vim' },

  -- }}}
  -- kensaku-search.vim {{{

  { 'lambdalisue/kensaku-search.vim' },

  -- }}}
  -- vim-highlightedyank {{{

  {
    'machakann/vim-highlightedyank',
    config = function()
      vim.g.highlightedyank_highlight_duration = 200
    end,
  },

  -- }}}
  -- yanky.nvim {{{

  {
    'gbprod/yanky.nvim',
    dependencies = { 'nvim-telescope/telescope.nvim' },
    enabled = InitLua.disable_yanky ~= true, -- because yanky.nvim is heavy
    config = function()
      require('yanky').setup({
        ring = {
          history_length = 100,
          storage = 'shada',
          sync_with_numbered_registers = true,
          cancel_event = 'update',
        },
        picker = {
          telescope = {
            mappings = {
              -- TODO: これ機能してる？
              -- TODO: なゆちゃんがとりあえず書いてくれたものなので、カスタマイズする
              default = require('yanky.picker').actions.put('p'),
              i = {
                ['<c-g>'] = require('yanky.picker').actions.put('P'),
                ['<c-x>'] = require('yanky.picker').actions.delete(),
                ['<c-r>'] = require('yanky.picker').actions.set_register(),
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
      { 'y', '<Plug>(YankyYank)', mode = { 'n', 'x' } },
      { 'p', '<Plug>(YankyPutAfter)', mode = { 'n', 'x' } },
      { 'P', '<Plug>(YankyPutBefore)', mode = { 'n', 'x' } },
      { ']p', '<Plug>(YankyPutIndentAfterLinewise)', mode = 'n' },
      { '[p', '<Plug>(YankyPutIndentBeforeLinewise)', mode = 'n' },
      { ']P', '<Plug>(YankyPutIndentAfterLinewise)', mode = 'n' },
      { '[P', '<Plug>(YankyPutIndentBeforeLinewise)', mode = 'n' },
      { '>p', '<Plug>(YankyPutIndentAfterShiftRight)', mode = 'n' },
      { '<p', '<Plug>(YankyPutIndentAfterShiftLeft)', mode = 'n' },
      { '>P', '<Plug>(YankyPutIndentBeforeShiftRight)', mode = 'n' },
      { '<P', '<Plug>(YankyPutIndentBeforeShiftLeft)', mode = 'n' },
      { '=p', '<Plug>(YankyPutAfterFilter)', mode = 'n' },
      { '=P', '<Plug>(YankyPutBeforeFilter)', mode = 'n' },
      { 'gy', '<CMD>Telescope yank_history<CR>', desc = 'Open Yank History' },

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
    config = function()
      vim.g.copilot_no_tab_map = true
    end,
  },

  -- }}}
  -- Align {{{

  { 'vim-scripts/Align' },

  -- }}}
  -- vim-scratch-buffer {{{

  load_local_or_upstream(InitLua.disable_scratch_buffer ~= true, {
    'aiya000/vim-scratch-buffer',
    dir = vim.fn.expand('~/Repository/vim-scratch-buffer'),
    config = function()
      vim.g.scratch_buffer_default_open_method = 'vsp'
      vim.g.scratch_buffer_use_default_keymappings = false
      vim.g.scratch_buffer_file_pattern = {
        when_file_buffer = vim.fn.expand('~/tmp/scratch-%d'),
        when_tmp_buffer = vim.fn.expand('/tmp/scratch-%d'),
      }
      vim.g.scratch_buffer_default_buffer_size = vim.v.null
      vim.g.scratch_buffer_auto_hide_buffer = {
        when_tmp_buffer = 1,
        when_file_buffer = 1,
      }
    end,
  }),

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
  -- rainbow {{{

  {
    'luochen1990/rainbow',
    config = function()
      vim.g.rainbow_active = 1
    end,
  },

  -- }}}
  -- flatten.nvim {{{

  { 'willothy/flatten.nvim' },

  -- }}}
  -- vim-go {{{

  { 'fatih/vim-go', ft = 'go' },

  -- }}}
  -- vim-javascript {{{

  { 'pangloss/vim-javascript', ft = 'javascript' },

  -- }}}
  -- typescript-vim {{{

  { 'leafgarland/typescript-vim', ft = 'typescript' },

  -- }}}
  -- vim-jsx-typescript {{{

  { 'peitalin/vim-jsx-typescript', ft = { 'typescript.tsx', 'javascript.jsx' } },

  -- }}}
  -- vim-tsx {{{

  { 'ianks/vim-tsx', ft = 'tsx' },

  -- }}}
  -- vim-vue {{{

  { 'posva/vim-vue', ft = 'vue' },

  -- }}}
  -- vim-toml {{{

  { 'cespare/vim-toml', ft = 'toml' },

  -- }}}
  -- vim-yaml {{{

  { 'stephpy/vim-yaml', ft = 'yaml' },

  -- }}}
  -- json5.vim {{{

  { 'gutenye/json5.vim', ft = 'json5' },

  -- }}}
  -- rust.vim {{{

  { 'rust-lang/rust.vim', ft = 'rust' },

  -- }}}
  -- vim-fish {{{

  { 'dag/vim-fish', ft = 'fish' },

  -- }}}
  -- vim-ps1 {{{

  { 'PProvost/vim-ps1', ft = 'ps1' },

  -- }}}
  -- bats.vim {{{

  { 'aliou/bats.vim', ft = 'bats' },

  -- }}}
  -- kotlin-vim {{{

  { 'udalov/kotlin-vim', ft = 'kotlin' },

  -- }}}
  -- vim-scala {{{

  { 'derekwyatt/vim-scala', ft = 'scala' },

  -- }}}
  -- haskell-vim {{{

  { 'neovimhaskell/haskell-vim', ft = 'haskell' },

  -- }}}
  -- ghci-syntax {{{

  { 'vmchale/ghci-syntax', ft = 'dot-ghci' },

  -- }}}
  -- vim-ghcid-quickfix {{{

  { 'aiya000/vim-ghcid-quickfix', ft = { 'haskell', 'happy', 'alex' } },

  -- }}}
  -- vim-ft-clojure {{{

  { 'thinca/vim-ft-clojure', ft = { 'clojure', 'lisp' } },

  -- }}}
  -- alex.vim {{{

  { 'vim-scripts/alex.vim', ft = 'alex' },

  -- }}}
  -- vim-gfm-syntax {{{

  { 'rhysd/vim-gfm-syntax', ft = 'markdown' },

  -- }}}
  -- vim-firestore {{{

  { 'delphinus/vim-firestore', ft = 'firestore' },

  -- }}}
  -- vim-graphql {{{

  { 'jparise/vim-graphql', ft = 'graphql' },

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
  -- neomru.vim {{{

  { 'Shougo/neomru.vim' },

  -- }}}
  -- editorconfig-vim {{{

  { 'editorconfig/editorconfig-vim' },

  -- }}}
  -- context_filetype.vim {{{

  {
    'Shougo/context_filetype.vim',
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

  -- }}}
  -- vim-precious {{{

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

  -- }}}
  -- fern.vim {{{

  {
    'lambdalisue/fern.vim',
    config = function()
      vim.g.fern_default_hidden = 1
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

      -- Autocmds
      -- Read local tsconfig by deno
      autocmds.add('FileType', function()
        local local_tsconfig = vim.fn.getcwd() .. '/tsconfig.json'
        if vim.fn.filereadable(local_tsconfig) == 1 then
          vim.g.ale_javascript_deno_lint_options = '--config ' .. local_tsconfig
        end
      end, { 'typescript', 'javascript' })

      autocmds.add('ColorScheme', function()
        vim.api.nvim_set_hl(0, 'ALEError', { ctermbg = 'gray', ctermfg = 'black' })
      end)
    end,
  },

  -- }}}
  -- deepl.vim {{{

  {
    'ryicoh/deepl.vim',
    config = function()
      vim.g.deepl_endpoint = 'https://api-free.deepl.com/v2/translate'
    end,
  },

  -- }}}
  -- vim-webpage {{{

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

  { 'skywind3000/asyncrun.vim' },

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
    config = function()
      vim.g.openbrowser_browser_commands = {
        { name = 'wslview', args = { '{browser}', '{uri}' } },
      }
    end,
  },

  -- }}}
}
