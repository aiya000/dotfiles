-- プラグイン設定

local helper = require('helper')
local fn = require('utils.functions')
local s = fn.s

-- vim-quickrun {{{

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
      'wslview %s.png', -- 今は仮で、WSLであると断定しておく。See: ↓TODOコメント
      -- TODO: InitLua.open_on_guiが正しく代入できてない。できてたら、このコメントを消して、↓をアンコメントする
      -- InitLua.open_on_gui .. ' %s.png',
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

-- }}}
-- foldCC {{{

vim.g.foldCCtext_maxchars = 120

-- }}}
-- vim-submode {{{

vim.g.submode_timeout = 0

-- Window resize submode
vim.call('submode#enter_with', 'win_resize', 'n', '', '<C-s>w')
vim.call('submode#map', 'win_resize', 'n', '', 'j', '3<C-w>+')
vim.call('submode#map', 'win_resize', 'n', '', 'k', '3<C-w>-')
vim.call('submode#map', 'win_resize', 'n', '', 'h', '3<C-w><')
vim.call('submode#map', 'win_resize', 'n', '', 'l', '3<C-w>>')
vim.call('submode#map', 'win_resize', 'n', '', '<', '20<C-w><')
vim.call('submode#map', 'win_resize', 'n', '', '>', '20<C-w>>')

-- Tab move submode
vim.call('submode#enter_with', 'tab_move', 'n', 's', '<C-s>n', ':<C-u>call vimrc#move_tab_next()<CR>')
vim.call('submode#enter_with', 'tab_move', 'n', 's', '<C-s>p', ':<C-u>call vimrc#move_tab_prev()<CR>')
vim.call('submode#map', 'tab_move', 'n', 's', 'n', ':<C-u>call vimrc#move_tab_next()<CR>')
vim.call('submode#map', 'tab_move', 'n', 's', 'p', ':<C-u>call vimrc#move_tab_prev()<CR>')

-- Window move submode
vim.call('submode#enter_with', 'win_move', 'n', 's', '<C-s>N', ':<C-u>call vimrc#move_window_forward()<CR>')
vim.call('submode#enter_with', 'win_move', 'n', 's', '<C-s>P', ':<C-u>call vimrc#move_window_backward()<CR>')
vim.call('submode#map', 'win_move', 'n', 's', 'N', ':<C-u>call vimrc#move_window_forward()<CR>')
vim.call('submode#map', 'win_move', 'n', 's', 'P', ':<C-u>call vimrc#move_window_backward()<CR>')
vim.call('submode#map', 'win_move', 'n', 'e', 'H', '"\\<C-w>H" .. (foldlevel(".") > 0 ? "zO" : "") .. "zz"')
vim.call('submode#map', 'win_move', 'n', 'e', 'J', '"\\<C-w>J" .. (foldlevel(".") > 0 ? "zO" : "") .. "zz"')
vim.call('submode#map', 'win_move', 'n', 'e', 'K', '"\\<C-w>K" .. (foldlevel(".") > 0 ? "zO" : "") .. "zz"')
vim.call('submode#map', 'win_move', 'n', 'e', 'L', '"\\<C-w>L" .. (foldlevel(".") > 0 ? "zO" : "") .. "zz"')
vim.call('submode#map', 'win_move', 'n', 's', '_', '<C-w>_')
vim.call('submode#map', 'win_move', 'n', 's', '"', ':resize 5<CR>')

-- }}}
-- aho-bakaup.vim {{{

vim.g.bakaup_backup_dir = InitLua.backupdir
vim.g.bakaup_auto_backup = 1

-- }}}
-- neosnippet.vim {{{

vim.g['neosnippet#snippets_directory'] = s('{neovim_home}/neosnippets', { neovim_home = InitLua.neovim_home })
vim.g['neosnippet#disable_select_select_mappings'] = 1

-- }}}
-- vim-textobj-indent {{{

vim.g.textobj_indent_no_default_key_mappings = 1

-- }}}
-- vim-visualstar {{{

-- Do zzzv after execute visualstar
vim.g.visualstar_extra_commands = 'zzzv'

-- }}}
-- vim-textobj-between {{{

vim.g.textobj_between_no_default_key_mappings = 1

-- }}}
-- ale {{{

-- ======
-- Common
-- ======
vim.g.ale_set_highlights = false
vim.g.ale_vim_vint_show_style_issues = false
vim.g.ale_virtualtext_cursor = 'current'

-- =======
-- Linters
-- =======

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
  'typescript',
  'javascript',
  'vue',
  'typescript.tsx',
  'javascript.jsx',
}

for _, ts in ipairs(typescript_variants) do
  vim.g.ale_linters[ts] = { 'prettier', 'eslint', 'vim-lsp' }
end

vim.g.ale_scala_scalastyle_config = vim.fn.expand('~/.dotfiles/scalastyle_config_default.xml')

-- ==========
-- Formatters
-- ==========

vim.g.ale_fix_on_save = true

vim.g.ale_fixers = {
  sh = { 'shfmt' },
  go = { 'gofmt', 'goimports' },
}

for _, ts in ipairs(typescript_variants) do
  vim.g.ale_fixers[ts] = { 'prettier', 'eslint' }
end

-- }}}
-- vim-highlightedyank {{{

vim.g.highlightedyank_highlight_duration = 200

-- }}}
-- vim-fmap {{{

vim.g.fmap_use_default_keymappings = false
vim.g.fmap_escape_keys = { '', '', '' }

-- }}}
-- vim-indent-guides {{{

vim.g.indent_guides_enable_on_vim_startup = 1
vim.g.indent_guides_start_level = 2
vim.g.indent_guides_default_mapping = 0
vim.g.indent_guides_guide_size = 1
vim.g.indent_guides_auto_colors = 0
vim.g.indent_guides_tab_guides = 0
vim.g.indent_guides_exclude_filetypes = {
  '',
  'adrone_home',
  'aref_web',
  'gitcommit',
  'happy',
  'haskell',
  'help',
  'man',
  'markdown',
  'review',
}

-- }}}
-- vim-lsp {{{

vim.g.lsp_async_completion = 1
vim.g.lsp_diagnostics_enabled = 0
vim.g.lsp_document_code_action_signs_enabled = 0

-- NOTE: To debug
vim.g.lsp_log_file = vim.fn.expand('~/vim-lsp.log')
vim.g.lsp_log_verbose = 1

-- }}}
-- vim-lsp-settings {{{

-- solargraph is temporary disabled because it occurs error at runtime (dein.vim?)
vim.g.lsp_settings = {
  solargraph = { disabled = 1 },
}

vim.g.lsp_settings_filetype_vue = { 'typescript-language-server', 'volar-server' }
vim.g.lsp_settings_filetype_typescript = { 'typescript-language-server', 'deno' }
vim.g.lsp_settings_filetype_javascript = { 'typescript-language-server', 'deno' }

-- }}}
-- translate.vim {{{

vim.g.translate_source = 'en'
vim.g.translate_target = 'ja'
vim.g.translate_winsize = 10

-- }}}
-- vim-precious {{{

vim.g.precious_enable_switch_CursorMoved = {
  ['*'] = false,
}

vim.g.precious_enable_switch_CursorMoved_i = vim.g.precious_enable_switch_CursorMoved
vim.g.precious_enable_switchers = {}
vim.g.textobj_precious_no_default_key_mappings = true

-- }}}
-- context_filetype.vim {{{

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

-- }}}
-- sync-term-cwd.vim {{{

vim.g.synctermcwd_cd_command = 'lcd'

-- }}}
-- vim-webpage {{{

vim.g.webpage_source = {
  weblio = 'http://ejje.weblio.jp/content/%s',
  stackage = 'https://www.stackage.org/lts-15.4/hoogle?q=%s',
}

-- }}}
-- vim-quickrepl {{{

vim.g.quickrepl_config = {
  vue = { 'tsx' },
  ['typescript.tsx'] = { 'tsx' },
  go = { 'gore' },
  ps1 = { 'powrshell', 'powershell.exe' },
}

vim.g.quickrepl_use_default_key_mapping = true
vim.g.quickrepl_enable_debug = true

-- }}}
-- open-browser.vim {{{

vim.g.openbrowser_browser_commands = {
  {
    name = 'wslview',
    args = { '{browser}', '{uri}' },
  },
}

-- }}}
-- previm {{{

vim.g.previm_code_language_show = 1
vim.g.previm_hard_line_break = 1

if InitLua.is_wsl then
  vim.g.previm_wsl_mode = true
  vim.g.previm_open_cmd = 'wslview'
end

-- }}}
-- fern.vim {{{

vim.g.fern_default_hidden = 1

-- }}}
-- copilot.vim {{{

vim.g.copilot_no_tab_map = true

-- }}}
-- ddu-source-lsp {{{

vim.g.ddu_source_lsp_clientName = 'vim-lsp'

-- }}}
-- rainbow {{{

vim.g.rainbow_active = 1

-- }}}
-- gin.vim {{{

vim.g.gin_proxy_editor_opener = 'vsplit'

-- }}}
-- deepl.vim {{{

vim.g.deepl_endpoint = 'https://api-free.deepl.com/v2/translate'

-- }}}
-- vim-scatch-buffer {{{

vim.g.scratch_buffer_default_open_method = 'vsp'
vim.g.scratch_buffer_default_buffer_size = nil
vim.g.scratch_buffer_use_default_keymappings = false
vim.g.scratch_buffer_file_pattern = {
  when_file_buffer = vim.fn.expand('~/tmp/scratch-%d'),
}

-- }}}
-- vim-write-sync {{{

vim.g.write_sync_echo_success_on_write = true

vim.g.write_sync_lists = {
  { '~/tmp/a', '~/tmp/b', '~/tmp/c' }, -- For test
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

-- }}}
-- lexima.vim {{{

vim.call('lexima#add_rule', { char = '<', input_after = '>' })
-- vim.call('lexima#add_rule', { char = '<', at = '\\%#\\>', leave = 1 })
-- vim.call('lexima#add_rule', { char = '<BS>', at = '\\<\\%#\\>', delete = 1 })
vim.call('lexima#add_rule', { char = '「', input_after = '」' })
vim.call('lexima#add_rule', { char = '（', input_after = '）' })
vim.call('lexima#add_rule', { char = '【', input_after = '】' })

-- }}}
-- quickpeek.vim {{{

vim.g.quickpeek_auto = true

-- }}}
-- vim-ghcid-quickfix {{{

-- TODO: Implement ghcid_quickfix equivalent
-- vim.g.ghcid_quickfix = {
--   showing = function(qf_bufnr)
--     -- Implementation needed
--   end,
-- }

-- }}}
-- vim-operator-surround {{{

vim.schedule(helper.setup_operator_surround)

-- }}}
-- catppuccin {{{

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

-- }}}
-- galaxyline.nvim {{{

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
      -- auto change color according the vim mode
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

-- }}}
-- bufferline.nvim {{{

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

-- }}}
-- flatten.nvim {{{

-- TODO: flatten.nvim has compatibility issues, temporarily disabled
--[[
require('flatten').setup({
  window = {
    open = 'alternate', -- How to open new files: 'current', 'alternate', 'tab', 'split', 'vsplit'
  },
  callbacks = {
    should_block = function(argv)
      -- Block if opening for editing (common git commit scenarios)
      -- Don't block if it's just for viewing or with specific flags
      return vim.tbl_contains(argv, '-c')
        or vim.tbl_contains(argv, '+')
        or vim.tbl_contains(argv, '--cmd')
    end,

    pre_open = function()
      -- Set working directory to the nested instance's directory
      local cwd = vim.fn.getcwd()
      vim.api.nvim_set_current_dir(cwd)
    end,

    post_open = function(bufnr, winnr, ft, is_blocking)
      -- Special handling for git commit files
      if ft == 'gitcommit' or ft == 'gitrebase' then
        -- Auto-close buffer when commit is complete
        vim.api.nvim_create_autocmd({'BufDelete', 'BufWipeout'}, {
          buffer = bufnr,
          callback = function()
            -- Optional: show a message
            vim.notify('Git operation completed', vim.log.levels.INFO)
          end,
          once = true,
        })
      end
    end,

    block_end = function()
      -- Called when a blocking session ends
      vim.notify('Nested nvim session ended', vim.log.levels.INFO)
    end,
  },

  -- Allow nested sessions for these file types (optional)
  allow_cmd_passthrough = true,

  -- Nest level limit (optional)
  nest_limit = 3,
})
--]]

-- }}}
