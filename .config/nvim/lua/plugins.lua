---プラグイン設定

local arrow = require('luarrow').arrow
local fn = require('utils.functions')
local helper = require('helper')
local list = require('utils.list')

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
      'nvim-telescope/telescope-github.nvim',
      'gbprod/yanky.nvim',
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
              -- TODO: fzf-native使ってるから？ 動いてない
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
        },
      })
      require('telescope').load_extension('fzf')
      require('telescope').load_extension('gh')
      require('telescope').load_extension('yank_history')
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

    dependencies = {
      'rcarriga/nvim-notify',
      'SmiteshP/nvim-navic',
      'hrsh7th/cmp-nvim-lsp',
    },

    config = function()
      -- unused functionがグレー化されるのを無効化
      vim.api.nvim_set_hl(0, 'DiagnosticUnnecessary', { fg = 'NONE', bg = 'NONE' })

      -- 診断表示の設定
      vim.diagnostic.config({
        virtual_text = true, -- 行末に診断テキストを表示
        signs = true, -- サインカラムに表示
        underline = false, -- 下線表示を無効化
        update_in_insert = false, -- インサートモード中は更新しない
        severity_sort = true, -- 重要度でソート
        float = {
          border = 'rounded',
          source = 'always',
          header = '',
          prefix = '',
        },
      })

      -- 各LSPサーバーの設定 {{{

      -- 共通設定
      local navic = require('nvim-navic')
      local capabilities_common = require('cmp_nvim_lsp').default_capabilities()

      -- ホバーウィンドウの設定を改善
      local function on_attach_common(client, bufnr)
        if client.server_capabilities.documentSymbolProvider then
          navic.attach(client, bufnr)
        end
      end

      vim.lsp.config('lua_ls', {
        capabilities = capabilities_common,
        on_attach = on_attach_common,
        settings = {
          Lua = {
            runtime = { version = 'LuaJIT' },
            workspace = { library = vim.api.nvim_get_runtime_file('', true) },
          },
        },
      })

      vim.lsp.config('ts_ls', {
        capabilities = capabilities_common,
        on_attach = on_attach_common,
      })

      -- See also mason-lspconfig.nvim section?
      -- }}}
    end,
  },

  -- }}}
  -- nvim-cmp {{{

  {
    'hrsh7th/nvim-cmp',
    dependencies = {
      'hrsh7th/cmp-nvim-lsp',
      'hrsh7th/cmp-buffer',
      'hrsh7th/cmp-path',
      'hrsh7th/cmp-cmdline',
      'hrsh7th/cmp-emoji',
      'L3MON4D3/LuaSnip',
      'saadparwaiz1/cmp_luasnip',
      'aiya000/nvim-luasnip-emoji',
    },
    config = function()
      ---@module 'cmp' -- Not working?
      local cmp = require('cmp')
      local luasnip = require('luasnip') ---@type any -- undefined-fieldエラーが出まくるのでとりあえずanyにする。ソースを読んだところ2025-10-07現在、LuaSnipには型が書かれていなかったので、これでいいと思う

      ---descがあればそれをkindの前に表示。
      ---例えば
      ---```
      ---emoji_pig   🐷 Snippet
      ---emoji_dog   🐶 Snippet
      ---emoji_cow   🐮 Snippet
      ---emoji_cat   🐱 Snippet
      ---emoji_ram   🐏 Snippet
      ---```
      ---のようになる。
      ---@param entry cmp.Entry
      ---@param vim_item vim.CompletedItem
      ---@return vim.CompletedItem
      local function format_entry_to_show_first_candidate(entry, vim_item)
        local snip = entry:get_completion_item()
        if snip.data ~= nil and snip.data.snip_id ~= nil then
          local snippet = luasnip.get_id_snippet(snip.data.snip_id)
          if snippet ~= nil and snippet.dscr ~= nil and snippet.dscr[1] ~= nil then
            vim_item.kind = snippet.dscr[1] .. ' ' .. vim_item.kind
          end
        end
        return vim_item
      end

      cmp.setup({
        snippet = {
          expand = function(args)
            luasnip.lsp_expand(args.body)
          end,
        },

        formatting = {
          format = function(entry, vim_item)
            if entry.source.name == 'luasnip' then
              return format_entry_to_show_first_candidate(entry, vim_item)
            end
            return vim_item
          end,
        },

        sources = cmp.config.sources({
          { name = 'nvim_lsp' },
          { name = 'luasnip' },
          { name = 'buffer' },
          { name = 'path' },
          { name = 'emoji' },
        }),

        -- NOTE: なぜかこれらを削除すると、nvim-cmpの補完候補が出た後に<C-n>キー・<C-p>キーを実行すると、Neovim標準の補完に落ちるので、削除しない
        mapping = cmp.mapping.preset.insert({
          ['<Tab>'] = cmp.mapping(function(fallback)
            if cmp.visible() then
              cmp.select_next_item()
            else
              fallback()
            end
          end, { 'i', 's' }),
          ['<S-Tab>'] = cmp.mapping(function(fallback)
            if cmp.visible() then
              cmp.select_prev_item()
            else
              fallback()
            end
          end, { 'i', 's' }),
        }),
      })

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
          function(_)
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
      end
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
  -- jaq-nvim {{{

    {
      'is0n/jaq-nvim',
      opts = {
        cmds = {
          internal = {
            lua = 'luafile %',
            vim = 'source %'
          },
          external = {
            -- markdown = 'glow %', -- 'after/ftplugin/markdown.lua'で管理
            typescript = 'bun run %',
          }
        },

        behavior = {
          default = 'float',
          startinsert = true, -- これやってると、すぐEnterで閉じられる
          wincmd = false,
          autosave = false,
        },

        ui = {
          float = {
            border = {'╔', '═' ,'╗', '║', '╝', '═', '╚', '║'} ,
            winhl     = 'Normal', -- See ':h winhl'
            borderhl  = 'FloatBorder',
            winblend  = 0, -- See ':h winblend'

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

  helper.load_from_local_or_remote(
    'aiya000/vim-neoquickrun',
    '~/Repository/vim-neoquickrun',
    InitLua.disable_neoquickrun == true,
    {
      init = function()
      end,
    }
  ),

  -- }}}
  -- hydra.nvim {{{

  {
    'anuvyklack/hydra.nvim',
    config = function()
      local Hydra = require('hydra')

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

      InitLua.hydra.yanky_ring = Hydra({
        name = 'yanky-ring',
        mode = 'n',
        heads = {
          { '<C-p>', '<Plug>(YankyPreviousEntry)', { private = true, desc = '↑' } },
          { '<C-n>', '<Plug>(YankyNextEntry)', { private = true, desc = '↓' } },
        },
      })
    end,
  },

  -- }}}
  -- bakaup.vim {{{

  helper.load_from_local_or_remote(
    'aiya000/bakaup.vim',
    '~/Repository/bakaup.vim',
    InitLua.disable_bakaup == true,
    {
      init = function()
        vim.g.bakaup_backup_dir = InitLua.backupdir
      end,
    }
  ),

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

      ---Scan filetypes in the given directory.
      ---The return array is possibly contains duplicated filetypes.
      ---@param directory string --This function collects filetypes from this directory
      ---@return string[] --Filetypes
      local function scan_filetypes_in_directory_but_can_duplicate(directory)
        local handler = vim.uv.fs_scandir(directory)
        if handler == nil then
          error('Failed to scan directory: ' .. directory)
        end

        local filetypes = {} ---@type string[]
        while true do
          local name, type = vim.uv.fs_scandir_next(handler)
          if name == nil then
            break
          elseif type == 'file' then
            local filetype = name:gsub('%.lua$', '')
            table.insert(filetypes, filetype)
          elseif type == 'directory' then
            local filetype = name
            table.insert(filetypes, filetype)
          else
            error(('Not suported file type: { type = "%s", name = "%s" }'):format(type, name))
          end
        end

        return filetypes
      end

      ---Scan filetypes in the given directory.
      ---The return array does not contain duplicated filetypes.
      ---See `scan_filetypes_in_directory_but_can_duplicate()` for about params and the return type.
      ---@param directory string
      ---@return string[]
      local function scan_filetypes_in_directory(directory)
        return vim.iter(scan_filetypes_in_directory_but_can_duplicate(directory)):fold({}, function(filetypes, filetype)
          return not vim.tbl_contains(filetypes, filetype) and list.append(filetypes, filetype) or filetypes
        end)
      end

      -- from_luaローダーでディレクトリを登録しても、スニペットが展開できないので、それぞれスニペットファイルを手動で登録
      -- TODO: ちゃんとfrom_luaローダーを使って動くようにする
      local snippets_dir = vim.fn.stdpath('config') .. '/lua/luasnippets'
      local filetypes = scan_filetypes_in_directory(snippets_dir) -- TODO: '_'キーを'all'キーに変換する
      for _, filetype in ipairs(filetypes) do
        local ok, snips = pcall(require, 'luasnippets.' .. filetype)
        if not ok then
          vim.notify(('Failed to load snippets: "%s" - %s'):format(filetype, snips), vim.log.levels.ERROR)
        elseif snips and snips.snippets and type(snips.snippets) == 'table' and #snips.snippets > 0 then
          ls.add_snippets(filetype, snips.snippets)
        end
      end
    end,
  },

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

  {
    'rhysd/vim-operator-surround',
    dependencies = { 'kana/vim-operator-user' },
    config = function()
      vim.schedule(function()
        -- Basic symbols excluding brackets () [] {} and ` for unique mappings
        local common_symbol_blocks =
          vim.iter(list.concat(list.char_range('!', "'"), { '*', '&', '_', '|', '~', ':', '/' }))
            :map(function(sym)
              return {
                block = { sym, sym },
                motionwise = { 'char', 'line', 'block' },
                keys = { sym },
              }
            end)
            :totable()
        local common_blocks = list.concat(common_symbol_blocks, {
          -- English
          { block = { '(', ')' }, motionwise = { 'char', 'line', 'block' }, keys = { '(', ')', 'p' } },
          { block = { '[', ']' }, motionwise = { 'char', 'line', 'block' }, keys = { ']', 'k' } },
          { block = { '{', '}' }, motionwise = { 'char', 'line', 'block' }, keys = { '{', '}', 'P' } },
          { block = { '<', '>' }, motionwise = { 'char', 'line', 'block' }, keys = { '<', '>', 'K' } },
          { block = { ' ', ' ' }, motionwise = { 'char', 'line', 'block' }, keys = { '  ' } },
          { block = { '`', '`' }, motionwise = { 'char', 'line', 'block' }, keys = { '`', 'b' } },
          -- 日本語
          { block = { '（', '）' }, motionwise = { 'char', 'line', 'block' }, keys = { 'jp' } },
          { block = { '「', '」' }, motionwise = { 'char', 'line', 'block' }, keys = { 'jk' } },
          { block = { '【', '】' }, motionwise = { 'char', 'line', 'block' }, keys = { 'jK' } },
          { block = { '『', '』' }, motionwise = { 'char', 'line', 'block' }, keys = { 'j-k' } },
        })

        local common_markdown_blocks = {
          { block = { '**', '**' }, motionwise = { 'char', 'block' }, keys = { 'B' } },
          { block = { '~~', '~~' }, motionwise = { 'char', 'block' }, keys = { '~' } },
          -- { block = { '__', '__' }, motionwise = { 'char', 'block'  } } -- あんまり使わないから定義しないでおく
          { block = { '```', '```' }, motionwise = { 'line', 'block' }, keys = { '```' } },
        }

        local html_blocks = {
          { block = { '<p>', '</p>' }, motionwise = { 'char' }, keys = { '[p' } },
          { block = { '<a>', '</a>' }, motionwise = { 'char' }, keys = { '[a' } },
          { block = { '<div>', '</div>' }, motionwise = { 'char' }, keys = { '[d' } },
          { block = { '<span>', '</span>' }, motionwise = { 'char' }, keys = { '[s' } },
          { block = { '<h1>', '</h1>' }, motionwise = { 'char' }, keys = { '[h1' } },
          { block = { '<h2>', '</h2>' }, motionwise = { 'char' }, keys = { '[h2' } },
          { block = { '<h3>', '</h3>' }, motionwise = { 'char' }, keys = { '[h3' } },
          { block = { '<h4>', '</h4>' }, motionwise = { 'char' }, keys = { '[h4' } },
          { block = { '<h5>', '</h5>' }, motionwise = { 'char' }, keys = { '[h5' } },
          { block = { '<ol>', '</ol>' }, motionwise = { 'char' }, keys = { '[ol' } },
          { block = { '<ul>', '</ul>' }, motionwise = { 'char' }, keys = { '[ul' } },
          { block = { '<li>', '</li>' }, motionwise = { 'char' }, keys = { '[li' } },
        }

        vim.g['operator#surround#blocks'] = {
          ['-'] = common_blocks,
          markdown = list.concat(common_markdown_blocks, html_blocks, {
            -- これらはHTMLではご法度なので、Markdownのみで使う（README.mdなどで使うと便利）
            { block = { '<b>', '</b>' }, motionwise = { 'char' }, keys = { '[b' } },
            { block = { '<code>', '</code>' }, motionwise = { 'char' }, keys = { '[c' } },
          }),
          html = html_blocks,
          lua = {
            { block = { '[[', ']]' }, motionwise = { 'char', 'line', 'block' }, keys = { '[', ']' } },
          },
          review = {
            { block = { '@<b>{', '}' }, motionwise = { 'char' }, keys = { 'B' } },
            { block = { '@<i>{', '}' }, motionwise = { 'char' }, keys = { 'i' } },
            { block = { '@<u>{', '}' }, motionwise = { 'char' }, keys = { 'u' } },
            { block = { '@<tt>{', '}' }, motionwise = { 'char' }, keys = { 't' } },
            { block = { '@<idx>{', '}' }, motionwise = { 'char' }, keys = { 'x' } },
            { block = { '@<ruby>{', ', ruby}' }, motionwise = { 'char' }, keys = { 'r' } },
            { block = { '@<code>{', '}' }, motionwise = { 'char' }, keys = { 'c' } },
            { block = { '@<mathcode>{', '}' }, motionwise = { 'char' }, keys = { 'm' } },
            { block = { '@<img>{', '}' }, motionwise = { 'char' }, keys = { '[i' } },
            { block = { '@<list>{', '}' }, motionwise = { 'char' }, keys = { '[l' } },
          },
          vue = html_blocks,
          ['typescript.tsx'] = html_blocks,
        }
      end)
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

  {
    'folke/flash.nvim',
    event = 'VeryLazy',
    config = function()
      require('flash').setup({
        labels = 'asdfghjklqwertyuiozxcvbnm', -- 'p' is excluded because it forcely pastes text
        jump = {
          nohlsearch = true,
        },
        modes = {
          char = {
            char_actions = function()
              -- ftは常に右、FTは常に左
              return {
                ['f'] = 'right',
                ['F'] = 'left',
                ['t'] = 'right',
                ['T'] = 'left',
              }
            end,
          },
        },
      })
    end,
    keys = {
      -- NOTE: ;で訓練中
      -- { 'vs', mode = { 'n', 'x', 'o' }, function() require('flash').jump() end, desc = 'Flash' },
      { ';', mode = { 'n', 'x', 'o' }, function() require('flash').jump() end, desc = 'Flash' },
      -- NOTE: g;で訓練中
      -- { 'vS', mode = { 'n', 'x', 'o' }, function() require('flash').treesitter() end, desc = 'Flash Treesitter' },
      { 'g;', mode = { 'n', 'x', 'o' }, function() require('flash').treesitter() end, desc = 'Flash Treesitter' },
    },
  },

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
    -- TODO: flash.nvimとバッティングしてるのか、`'f.`ーでflash.nvimのエラーが出るので、調査する
    -- init = function()
    --   vim.g.fmap_use_default_keymappings = false
    --   vim.g.fmap_escape_keys = { '', '', '' }
    --   vim.cmd('FNoreMap / ・')
    --   vim.cmd('FNoreMap T ・')
    --   vim.cmd('FNoreMap tt …')
    --   vim.cmd("FNoreMap '' 　")
    --   vim.cmd('FNoreMap p （')
    --   vim.cmd('FNoreMap k 「')
    --   vim.cmd('FNoreMap K 〈')
    --   vim.cmd('FNoreMap -k 『')
    -- end,
  },

  -- }}}
  -- kensaku.vim {{{

  { 'lambdalisue/kensaku.vim' },

  -- }}}
  -- kensaku-search.vim {{{

  { 'lambdalisue/kensaku-search.vim' },

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
    config = function()
      vim.g.copilot_no_tab_map = true
    end,
  },

  -- }}}
  -- claudecode.nvim {{{

  fn.pipe('<leader>cc')
    :apply(function(toggle_key)
      ---Checks if window is actually displayed (has height/width)
      ---@param win integer --An element of `vim.api.nvim_list_wins()`
      local function is_win_displayed(win)
        local win_config = vim.api.nvim_win_get_config(win)
        return win_config.hide
          or (win_config.height ~= nil and win_config.height == 0)
          or (win_config.width ~= nil and win_config.width == 0)
      end

      ---Does the buffer typically contain 'claude' in its name ClaudeCode terminal?
      ---@param win integer --An element of `vim.api.nvim_list_wins()`
      local function is_buffer_name_claude(win)
        local buf = vim.api.nvim_win_get_buf(win)
        local bufname = vim.api.nvim_buf_get_name(buf)
        return bufname:match('claude') or vim.bo[buf].filetype == 'claudecode'
      end

      ---NOTE: なぜか`:ClaudeCodeFocus`でfloat windowがトグルしないので、ワークアラウンド
      ---Checks if ClaudeCode window/buffer is currently visible (not hidden)
      local function is_claudecode_window_opening()
        return vim.iter(vim.api.nvim_list_wins())
          :any(function(win) ---@param win integer
            return vim.api.nvim_win_is_valid(win)
              and not is_win_displayed(win)
              and is_buffer_name_claude(win)
          end)
      end

      local function toggle()
        if is_claudecode_window_opening() then
          vim.cmd('ClaudeCodeFocus')
          return
        end

        local cwd = vim.fn.getcwd()
        fn.try_finally(function()
          vim.cmd('lcd ' .. InitLua.path_at_started)
          vim.cmd('ClaudeCode --continue')
          return nil
        end, function()
          vim.cmd('lcd ' .. cwd)
        end)
      end

      -- TODO: トグルになってないので、トグルする。その際は<leader>ccとは別個のトグルにする
      local function toggle_docker()
        vim.cmd('ClaudeCodeDocker')
      end

      return {
        'coder/claudecode.nvim',
        dependencies = { 'folke/snacks.nvim' },
        config = true,
        cmd = {
          'ClaudeCode',
          'ClaudeCodeFocus',
          'ClaudeCodeSelectModel',
          'ClaudeCodeAdd',
          'ClaudeCodeSend',
          'ClaudeCodeTreeAdd',
          'ClaudeCodeDiffAccept',
          'ClaudeCodeDiffDeny',
        },
        keys = {
          {
            toggle_key,
            mode = { 'n' },
            toggle,
            desc = 'Toggle Claude Code',
          },
          { '<leader>cr', mode = { 'n' }, '<Cmd>ClaudeCode --resume<CR>', desc = 'Resume Claude' },
          { '<leader>cC', mode = { 'n' }, '<Cmd>ClaudeCode<CR>', desc = 'New Claude' },
          { '<leader>cD', mode = { 'n' }, toggle_docker, desc = 'Toggle Docker Claude' },
          { '<leader>cM', mode = { 'n' }, '<Cmd>ClaudeCodeSelectModel<CR>', desc = 'Select Claude model' },
          { '<leader>cb', mode = { 'n' }, '<Cmd>ClaudeCodeAdd %<CR>', desc = 'Add current buffer' },
          { '<leader>cs', mode = { 'n' }, 'V:ClaudeCodeSend<CR>', desc = 'Send to Claude' },
          { '<leader>cs', mode = { 'v' }, '<Cmd>ClaudeCodeSend<CR>', desc = 'Send to Claude' },
          {
            '<leader>cs',
            '<Cmd>ClaudeCodeTreeAdd<CR>',
            desc = 'Add file',
            ft = { 'NvimTree', 'neo-tree', 'oil', 'minifiles' },
          },
          { '<leader>ca', '<Cmd>ClaudeCodeDiffAccept<CR>', desc = 'Accept diff' },
          { '<leader>cd', '<Cmd>ClaudeCodeDiffDeny<CR>', desc = 'Deny diff' },
        },
        opts = {
          -- Open in a floating window
          terminal = {
            ---@module 'snacks'
            ---@type snacks.win.Config | {}
            snacks_win_opts = {
              position = 'float',
              width = 0.9,
              height = 0.9,
              border = 'rounded',
              keys = {
                claude_hide = {
                  toggle_key,
                  function(self)
                    self:hide()
                  end,
                  mode = 't',
                  desc = 'Hide',
                },
              },
            },
          },
          -- TODO: これちゃんと動いてる？ チェックする
          diff_opts = {
            auto_close_on_accept = true,
            vertical_split = true,
            open_in_current_tab = true,
            keep_terminal_focus = false,
          },
        },
      }
    end)
    :get(),

  -- }}}
  -- Align {{{

  { 'vim-scripts/Align' },

  -- }}}
  -- nvim-mado-scratch-buffer {{{

  helper.load_from_local_or_remote(
    'aiya000/nvim-mado-scratch-buffer',
    '~/Repository/nvim-mado-scratch-buffer',
    InitLua.disable_scratch_buffer == true,
    {
      opts = {
        file_pattern = {
          when_file_buffer = vim.fn.expand('~/tmp/scratch-%d'),
        },
        default_file_ext = 'md',
        default_open_method = 'vsp',
        -- default_open_method = {
        --   method = 'float',
        --   size = {
        --     width = 100,
        --     height = 40,
        --   },
        -- },
        default_buffer_size = 'no-auto-resize',
      },
    }
  ),

  -- }}}
  -- nvim-just-stay-search {{{

  helper.load_from_local_or_remote(
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
          vim.call('vimrc#popup_atcursor', ':AsyncRun finished')
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

  {
    'hachy/cmdpalette.nvim',
    dependencies = {
      'hrsh7th/nvim-cmp',
      'hrsh7th/cmp-cmdline',
    },
    config = function()
      require('cmdpalette').setup({
        buf = {
          filetype = 'cmdpalette',
        },
      })

      require('cmp').setup.filetype('cmdpalette', {
        sources = {
          { name = 'cmdline' },
        },
      })

      local augroup = vim.api.nvim_create_augroup('InitLuaPluginsCmdpalette', { clear = true })

      vim.api.nvim_create_autocmd('FileType', {
        group = augroup,
        pattern = 'cmdpalette',
        callback = function()
          vim.api.nvim_create_autocmd('TextChangedI', {
            group = augroup,
            buffer = 0,
            callback = function()
              -- cnoremapのcmdpalette版
              helper.replace_line(
                {
                  ['l '] = 'lua ',
                  ['lp'] = 'lua = ',
                  ['h '] = 'Telescope help_tags<CR>',
                  ['ev'] = ('e %s/'):format(InitLua.path_at_started),
                  ['eb'] = function()
                    return ('e %s/'):format(vim.fn.expand('%:p:h'))
                  end,
                  ['eg'] = function()
                    return ('e %s/'):format(InitLua.git_root) -- helper.git_rootは代入が遅延されるので、評価も遅延
                  end,
                  [':'] = '<Esc>', -- ::でcmdpaletteに突入した場合、normal-modeで突入するように
                },
                vim.api.nvim_get_current_line(),
                function(rhs)
                  vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes(rhs, true, false, true), 'i', false)
                end
              )
            end,
          })

          -- cmdpaletteバッファを閉じた後にエラーが出るので無効化
          vim.b.ale_enabled = 0

          vim.keymap.set('n', '<C-l>', '"yyy<Esc>', { remap = true, buffer = true }) -- 誤爆でEscapeすることがよくあるので、@zにバックアップ
          vim.keymap.set('n', '<C-j>', '<CR>', { remap = true, buffer = true })
          -- TODO: これで実行した場合、結果をhelper.open_buffer_to_execute()で開くようにする
          -- vim.keymap.set('n', '<C-k><C-j>', function()
          --   local line = vim.api.nvim_get_current_line()
          -- end, { remap = true, buffer = true })

          vim.keymap.set('i', '<C-j>', '<Esc><CR>', { remap = true, buffer = true }) -- <Esc> to hide completion menu

          vim.keymap.set('i', '<C-n>', function()
            require('cmp').complete()
          end, { buffer = true })
        end,
      })
    end,
    keys = {
      { ':', '<Cmd>Cmdpalette<CR>', desc = 'Open cmdpalette' },
    },
  },

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
      require('dial.config').augends:register_group{
        default = {
          augend.integer.alias.decimal,
          augend.integer.alias.hex,
          augend.date.alias['%Y/%m/%d'],
          augend.constant.alias.bool,
        },
      }
    end
  },

  -- }}}
  -- incline.nvim {{{

  {
    'b0o/incline.nvim',
    dependencies = {
      'nvim-tree/nvim-web-devicons',
      'SmiteshP/nvim-navic',
      'stevearc/oil.nvim',
    },
    config = function()
      local helpers = require('incline.helpers')
      local navic = require('nvim-navic')
      local devicons = require('nvim-web-devicons')

      require('incline').setup({
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

        render = function(props)
          local function get_oil_current_dir_renderer(buf)
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

          local function get_file_renderer(buf, focused)
            local filename = vim.api.nvim_buf_get_name(buf)
              % arrow(function(bufname)
                return vim.fn.fnamemodify(bufname, ':t')
              end)
              ^ arrow(function(filename)
                return filename == '' and '[No Name]' or filename
              end)

            local ft_icon, ft_color = devicons.get_icon_color(filename)
            local modified = vim.bo[buf].modified
            local res = {
              ft_icon and { ' ', ft_icon, ' ', guibg = ft_color, guifg = helpers.contrast_color(ft_color) } or '',
              ' ',
              { filename, gui = modified and 'bold,italic' or 'bold' },
              guibg = '#afafff',
              guifg = '#000000',
            }

            if focused then
              table.insert(res, navic.get_data(buf) or {}) -- パンくずリスト
            end

            table.insert(res, ' ')
            return res
          end

          if vim.bo[props.buf].filetype == 'oil' then
            return get_oil_current_dir_renderer(props.buf)
          end
          return get_file_renderer(props.buf, props.focused)
        end,
      })
    end,
  },

  -- }}}
  -- nvim-navic {{{

  {
    'SmiteshP/nvim-navic',
    dependencies = {
      'neovim/nvim-lspconfig',
    },
  },

  -- }}}
  -- nvim-luasnip-emoji {{{

  helper.load_from_local_or_remote(
    'aiya000/nvim-luasnip-emoji',
    '~/Repository/nvim-luasnip-emoji',
    InitLua.disable_luasnip_emoji == true,
    {
      dependencies = {
        'L3MON4D3/LuaSnip',
      },
    }
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

          local distance_to_scroll = distance > 0
            and math.min(distance, 100) -- Example: max(200, 100)
            or math.max(distance, -100) -- Example: min(-200, -100)
          neoscroll.scroll(distance_to_scroll, keymaps_opts)
          vim.defer_fn(function()
            vim.notify('poi: ' .. vim.inspect(count), vim.log.levels.INFO)
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
      version = "*",
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

      post_open_float = function(_)
        -- Because it disables by default
        vim.opt.number = true
        vim.opt.relativenumber = true
      end
    },
    ft = { 'markdown', 'html' },
  },

  -- }}}
  -- pantran.nvim {{{

  {
    'potamides/pantran.nvim',
    dependencies = { 'nvim-lua/plenary.nvim' },
    config = function()
      local pantran = require('pantran')

      pantran.setup({
        default_engine = 'deepl',
        ui = {
          width_percentage = 0.8,
          height_percentage = 0.9,
        },
        engines = {
          deepl = {
            auth_key = vim.env.NVIM_DEEPL_AUTH_KEY,
            free_api = true,
          },
        },
      })

      ---@param target_lang string
      local function create_opening_with_current_word(target_lang)
        return function()
          local word = vim.fn.expand('<cword>')
          vim.cmd('Pantran target=' .. target_lang)

          vim.schedule(function()
            vim.api.nvim_paste(word, false, -1)
            vim.keymap.set('n', '?', 'g?', { buffer = true, remap = true })
            vim.keymap.set('n', '<C-k><C-j>', 'gy', { buffer = true, remap = true })
            vim.keymap.set('n', '<C-l>', 'q', { buffer = true, remap = true })
          end)
        end
      end

      ---@param target_lang string
      local function create_opening_with_selected_line(target_lang)
        return function()
          local start_line = vim.fn.line('v')
          local end_line = vim.fn.line('.')
          -- 逆順の場合もあるので、小さい方をstart_lineに
          if start_line > end_line then
            start_line, end_line = end_line, start_line
          end

          local lines = vim.api.nvim_buf_get_lines(0, start_line - 1, end_line, false)
          local selected_text = table.concat(lines, '\n')
          vim.cmd('Pantran target=' .. target_lang)

          vim.schedule(function()
            vim.api.nvim_paste(selected_text, false, -1)
            vim.keymap.set('n', '?', 'g?', { buffer = true, remap = true })
            vim.keymap.set('n', '<C-k><C-j>', 'gy', { buffer = true, remap = true })
            vim.keymap.set('n', '<C-l>', 'q', { buffer = true, remap = true })
          end)
        end
      end

      vim.keymap.set('n', '<leader>k', create_opening_with_current_word('JA'))
      vim.keymap.set('n', '<leader>K', create_opening_with_current_word('EN-US'))
      vim.keymap.set('v', '<leader>k', create_opening_with_selected_line('JA'))
      vim.keymap.set('v', '<leader>K', create_opening_with_selected_line('EN-US'))
    end,
  },

  -- }}}
  -- colorful-winsep.nvim {{{

  {
    'nvim-zh/colorful-winsep.nvim',
    config = true,
    event = { 'WinLeave' },
  },

  -- }}}
  -- oil.nvim {{{

  {
    'stevearc/oil.nvim',
    lazy = false,
    dependencies = { 'nvim-tree/nvim-web-devicons' },
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
}
