---プラグイン設定

local fn = require('utils.functions')
local list = require('utils.list')
local nvim = require('nvim')
local lightweight = require('plugins-lightweight')

return list.concat(lightweight, {
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

            return ' '
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
  -- vim-neoquickrun {{{

  nvim.load_from_local_or_remote(
    'aiya000/vim-neoquickrun',
    '~/Repository/vim-neoquickrun',
    InitLua.disable_neoquickrun == true,
    {
      init = function()
        vim.keymap.set({ 'n', 'v' }, '<leader>r', '<Plug>(neoquickrun)', { nowait = true }) -- TODO: <leader>r以降のキーマッピングを取っているのはだれ？
      end,
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
  -- async.vim {{{

  { 'prabirshrestha/async.vim' },

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
  -- vim-textobj-xmlattr {{{

  {
    'whatyouhide/vim-textobj-xmlattr',
    dependencies = { 'kana/vim-textobj-user' },
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
  -- adrone.vim {{{

  {
    'aiya000/adrone.vim',
    cmd = { 'AdroneHome', 'AdroneSay', 'AdroneVersion' },
  },

  -- }}}
  -- vim-themis {{{

  { 'thinca/vim-themis', ft = { 'vim', 'vimspec' } },

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
  -- nvim-cmp {{{

  -- TODO: DRY - `./plugins-lightweight.lua`と重複している記述を、モジュール分割などでリファクタリングし、削除
  {
    'hrsh7th/nvim-cmp',
    config = function()
      if not InitLua.recording_mode then
        local cmp = require('cmp')
        local common_mapping = {
          ['<C-n>'] = cmp.mapping.select_next_item(),
          ['<C-p>'] = cmp.mapping.select_prev_item(),
          ['<C-i>'] = cmp.mapping.select_next_item(),
          ['<Tab>'] = cmp.mapping.select_next_item(),
          ['<CR>'] = cmp.mapping.confirm({ select = false }),
        }

        -- In full mode, plugins-lightweight's cmp config is overridden by this block,
        -- so we need to call cmp.setup() here as well.
        if not vim.g.nvim_lightweight_mode then
          local luasnip = require('luasnip')
          cmp.setup({
            snippet = { expand = function(args) luasnip.lsp_expand(args.body) end },
            mapping = cmp.mapping.preset.insert(common_mapping),
            sources = cmp.config.sources({ { name = 'nvim_lsp' }, { name = 'luasnip' } }, { { name = 'buffer' }, { name = 'path' } }),
          })
          cmp.setup.cmdline({ '/', '?' }, {
            mapping = cmp.mapping.preset.cmdline(),
            sources = { { name = 'buffer' } },
          })
          cmp.setup.cmdline(':', {
            mapping = cmp.mapping.preset.cmdline(),
            sources = cmp.config.sources({ { name = 'path' } }, { { name = 'cmdline' } }),
            matching = { disallow_symbol_nonprefix_matching = false },
          })
        end

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
        performance_mode = false,
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
    opts = {
      highlight = '#cba6f7',
    },
    event = { 'WinLeave' },
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
  -- luarrow {{{

  {
    'aiya000/luarrow.lua',
    build = 'luarocks install --lua-version 5.1 luarrow',
  },

  -- }}}
  -- chotto {{{

  {
    'aiya000/chotto.lua',
    build = 'luarocks install --lua-version 5.1 chotto',
  },

  -- }}}
})

-- vim: set foldmethod=marker foldlevel=1:
