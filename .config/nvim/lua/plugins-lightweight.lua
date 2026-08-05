---lightweight版プラグイン設定
---init_lightweight.lua から使われる。
---plugins.lua のサブセット。

local nvim = require('nvim-lightweight')

return {
  -- catppuccin {{{

  {
    'catppuccin/nvim',
    name = 'catppuccin',
    priority = 1000,
    config = function()
      require('catppuccin').setup({
        flavour = 'mocha',
        background = { light = 'latte', dark = 'mocha' },
        transparent_background = false,
        show_end_of_buffer = false,
        term_colors = false,
        dim_inactive = { enabled = false, shade = 'dark', percentage = 0.15 },
        no_italic = false,
        no_bold = false,
        styles = {
          comments = { 'italic' },
          conditionals = { 'italic' },
          loops = {}, functions = {}, keywords = {}, strings = {},
          variables = {}, numbers = {}, booleans = {}, properties = {},
          types = {}, operators = {},
        },
        color_overrides = {},
        custom_highlights = {},
        integrations = {
          cmp = true, gitsigns = true, nvimtree = true,
          telescope = true, notify = false, mini = false,
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
          preview = { treesitter = false },
          mappings = {
            n = {
              ['<C-l>'] = actions.close,
              ['<C-j>'] = actions.select_default,
            },
            i = {
              ['<C-l>'] = function() nvim.run_with_virtual_keymaps('<Esc>') end,
              ['<C-j>'] = actions.select_default,
              ['<Esc>'] = function() nvim.run_with_virtual_keymaps('<Esc>') end,
              ['<C-b>'] = function() nvim.run_with_virtual_keymaps('<Left>') end,
              ['<C-f>'] = function() nvim.run_with_virtual_keymaps('<Right>') end,
              ['<C-a>'] = function() nvim.run_with_virtual_keymaps('<Home>') end,
              ['<C-e>'] = function() nvim.run_with_virtual_keymaps('<End>') end,
              ['<C-h>'] = function() nvim.run_with_virtual_keymaps('<Backspace>') end,
              ['<C-d>'] = function() nvim.run_with_virtual_keymaps('<Delete>') end,
            },
          },
        },
        extensions = {
          frecency = { db_safe_mode = false },
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
        render = 'background',
        enable_named_colors = true,
        enable_tailwind = false,
      })
    end,
  },

  -- }}}
  -- telescope-fzf-native {{{

  {
    'nvim-telescope/telescope-fzf-native.nvim',
    build = 'make',
    cond = function() return vim.fn.executable('make') == 1 end,
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
        timeout = 1000000,
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
        chunk = { enable = true, style = '#c678dd' },
        indent = { enable = true },
        line_num = { enable = true },
        blank = { enable = false },
      })
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

  { 'nvim-lua/plenary.nvim', lazy = false },

  -- }}}
  -- snacks.nvim {{{

  { 'folke/snacks.nvim' },

  -- }}}
  -- vim-denops/denops.vim {{{

  { 'vim-denops/denops.vim', lazy = false },

  -- }}}
  -- nvim-treesitter {{{

  { 'nvim-treesitter/nvim-treesitter', build = ':TSUpdate' },

  -- }}}
  -- lexima.vim {{{

  {
    'cohama/lexima.vim',
    enabled = not InitLua.recording_mode,
    event = 'InsertEnter',
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

  { 'osyo-manga/vim-textobj-from_regexp', dependencies = { 'kana/vim-textobj-user' } },

  -- }}}
  -- vim-textobj-jabraces {{{

  { 'kana/vim-textobj-jabraces', dependencies = { 'kana/vim-textobj-user' } },

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

  { import = 'plugins.vim-operator-surround' },

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
  -- kensaku.vim {{{

  { 'lambdalisue/kensaku.vim' },

  -- }}}
  -- kensaku-search.vim {{{

  {
    'lambdalisue/kensaku-search.vim',
    dependencies = { 'lambdalisue/kensaku.vim' },
    config = function()
      nvim.keymaps_set('c', { '<CR>', '<C-m>', '<C-j>' }, '<Plug>(kensaku-search-replace)<CR>')
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
        highlight = { on_put = true, on_yank = true, timer = 200 },
        preserve_cursor_position = { enabled = true },
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
    },
  },

  -- }}}
  -- vim-scripts/Align {{{

  { 'vim-scripts/Align' },

  -- }}}
  -- nvim-just-stay-search {{{

  nvim.load_from_local_or_remote(
    'aiya000/nvim-just-stay-search',
    '~/Repository/nvim-just-stay-search',
    InitLua.disable_just_stay_search == true,
    { config = function() require('just-stay-search').setup() end }
  ),

  -- }}}
  -- vim-write-sync {{{

  {
    'aiya000/vim-write-sync',
    config = function()
      vim.g.write_sync_echo_success_on_write = true
      vim.g.write_sync_lists = {
        { '~/tmp/a', '~/tmp/b', '~/tmp/c' },
        { '~/.dotfiles/Windows/Preferences/AutoHotkey.ahk', '~/Desktop/AutoHotkey.ahk' },
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
        query = { [''] = 'rainbow-delimiters', lua = 'rainbow-blocks' },
        highlight = {
          'RainbowDelimiterRed', 'RainbowDelimiterYellow', 'RainbowDelimiterBlue',
          'RainbowDelimiterOrange', 'RainbowDelimiterGreen', 'RainbowDelimiterViolet',
          'RainbowDelimiterCyan',
        },
      }
    end,
  },

  -- }}}
  -- filetype syntax {{{

  { 'cespare/vim-toml', ft = 'toml' },
  { 'stephpy/vim-yaml', ft = 'yaml' },
  { 'aliou/bats.vim', ft = 'bats' },
  { 'vim-scripts/alex.vim', ft = 'alex' },
  { 'rhysd/vim-gfm-syntax', ft = 'markdown' },
  { 'vim-scripts/ShaderHighLight', ft = 'shaderlab' },
  { 'aiya000/vim-review', ft = 'review' },

  -- }}}
  -- editorconfig-vim {{{

  { 'editorconfig/editorconfig-vim' },

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

  {
    'akinsho/toggleterm.nvim',
    opts = {
      hide_numbers = true, shade_filetypes = {}, shade_terminals = true,
      shading_factor = 2, start_in_insert = true, insert_mappings = true,
      persist_size = true, direction = 'float', close_on_exit = true,
      shell = vim.o.shell,
      float_opts = {
        border = 'curved', winblend = 0,
        highlights = { border = 'Normal', background = 'Normal' },
      },
    },
  },

  -- }}}
  -- nui.nvim {{{

  { 'MunifTanjim/nui.nvim' },

  -- }}}
  -- monaqa/dial.nvim {{{

  {
    'monaqa/dial.nvim',
    config = function()
      vim.keymap.set('n', '<C-a>', function() require('dial.map').manipulate('increment', 'normal') end)
      vim.keymap.set('n', '<C-x>', function() require('dial.map').manipulate('decrement', 'normal') end)
      vim.keymap.set('n', 'g<C-a>', function() require('dial.map').manipulate('increment', 'gnormal') end)
      vim.keymap.set('n', 'g<C-x>', function() require('dial.map').manipulate('decrement', 'gnormal') end)
      vim.keymap.set('x', '<C-a>', function() require('dial.map').manipulate('increment', 'visual') end)
      vim.keymap.set('x', '<C-x>', function() require('dial.map').manipulate('decrement', 'visual') end)
      vim.keymap.set('x', 'g<C-a>', function() require('dial.map').manipulate('increment', 'gvisual') end)
      vim.keymap.set('x', 'g<C-x>', function() require('dial.map').manipulate('decrement', 'gvisual') end)

      local augend = require('dial.augend')
      require('dial.config').augends:register_group({
        default = {
          augend.integer.alias.decimal,
          augend.integer.alias.hex,
          augend.date.alias['%Y/%m/%d'],
          augend.date.alias['%Y-%m-%d'],
          augend.constant.alias.bool,
          augend.constant.new({ elements = { '[ ]', '[x]' }, word = false, cyclic = true }),
          augend.constant.new({
            elements = { '作業中', 'レビュー待ち', 'レビュー対応中', '再レビュー待ち', '再レビュー対応中', 'マージ済み' },
            word = false, cyclic = true,
          }),
          augend.constant.new({ elements = { 'yes', 'no' }, word = false, cyclic = true }),
        },
      })
    end,
  },

  -- }}}
  -- nvim-cmp {{{

  {
    'hrsh7th/nvim-cmp',
    enabled = not InitLua.recording_mode,
    event = { 'InsertEnter', 'CmdlineEnter' },
    dependencies = {
      'hrsh7th/cmp-nvim-lsp', 'hrsh7th/cmp-buffer', 'hrsh7th/cmp-path',
      'hrsh7th/cmp-cmdline', 'L3MON4D3/LuaSnip', 'saadparwaiz1/cmp_luasnip',
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
      ls.config.set_config({
        history = true,
        updateevents = 'TextChanged,TextChangedI',
        ext_opts = {
          [types.insertNode] = {
            active = { virt_text = { { '', 'LuasnipInsertNodeActive' } }, virt_text_pos = 'inline', hl_mode = 'combine' },
            passive = { virt_text = { { '○', 'LuasnipInsertNodePassive' } }, virt_text_pos = 'inline', hl_group = 'LuasnipInsertNodePassive', hl_mode = 'combine' },
          },
          [types.exitNode] = {
            active = { virt_text = { { '', 'LuasnipInsertNodePassive' } }, virt_text_pos = 'inline', hl_mode = 'combine' },
            passive = { virt_text = { { '◀', 'LuasnipInsertNodePassive' } }, virt_text_pos = 'inline', hl_mode = 'combine' },
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
          H = { '-', mode = 'n', remap = true },
        },
        view_options = { show_hidden = true },
      })
    end,
  },

  -- }}}
  -- render-markdown.nvim は除外（重いため） ---
  -- telescope-heading.nvim {{{

  {
    'crispgm/telescope-heading.nvim',
    config = function()
      local augroup = vim.api.nvim_create_augroup('InitLuaPluginsTelescopeHeading', { clear = true })
      vim.api.nvim_create_autocmd('FileType', {
        group = augroup,
        pattern = { 'markdown', 'help', 'asciidoc' },
        callback = function()
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
  -- claudecode.nvim {{{

  { import = 'plugins.claudecode-nvim' },

  -- }}}
  -- cmdpalette.nvim {{{

  { import = 'plugins.cmdpalette-nvim' },

  -- }}}
  -- flash.nvim {{{

  { import = 'plugins.flash-nvim' },

  -- }}}
  -- nvim-mado-scratch {{{

  { import = 'plugins.nvim-mado-scratch' },

  -- }}}
  -- nvim-web-devicons {{{

  { 'nvim-tree/nvim-web-devicons' },

  -- }}}
  -- vim-fmap {{{

  {
    'aiya000/vim-fmap',
    cmd = 'FNoreMap',
    keys = {
      { '<Plug>(fmap-forward-f)', mode = { 'n', 'v' } },
      { '<Plug>(fmap-backward-f)', mode = { 'n', 'v' } },
      { '<Plug>(fmap-forward-t)', mode = { 'n', 'v' } },
      { '<Plug>(fmap-backward-T)', mode = { 'n', 'v' } },
    },
  },

  -- }}}
}

-- vim: set foldmethod=marker foldlevel=1:
