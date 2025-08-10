-- プラグイン設定

local s = require('utils.functions').s

-- vim-quickrun {{{
vim.g.quickrun_no_default_key_mappings = 0

vim.g.quickrun_config = {
  ['_'] = {
    -- Global Config
  },
  java = {
    cmdopt = '-encoding UTF-8 -source 1.8',
  },
  cs = {
    command = 'mcs',
  },
  vimspec = {
    command = 'themis',
    cmdopt = '--runtimepath ".."',
    exec = '%c %o %s:p | tr -d "\\r"',
    tempfile = vim.g.vimrc.vim_home .. '/{tempname()}.vimspec',
  },
  html = {
    command = vim.g.vimrc.open_on_gui,
    outputter = 'null',
    exec = '%c %s:p',
  },
  tex = {
    command = 'ptex2pdf',
    cmdopt = '-l',
    exec = '%c %o %s:r',
  },
  clojure = {
    command = 'lein',
    cmdopt = 'exec',
  },
  swift = {
    command = 'swift',
  },
  scala = {
    cmdopt = '-feature',
  },
  typescript = {
    command = 'ts-node',
    exec = { '%c %o %s' },
    cmdopt = '',
    tempfile = '%{tempname()}.ts',
  },
  brainfuck = {
    command = 'brainfuck',
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
  stack_test = {
    command = 'stack',
    cmdopt = 'test',
    exec = '%c %o',
    runner = 'vimproc',
    outputter = 'quickfix',
  },
  stack_build = {
    type = 'stack_test',
    cmdopt = 'build',
  },
  eta = {
    runner = 'vimproc',
  },
  etlas_build = {
    command = 'etlas',
    cmdopt = 'build',
    exec = '%c %o',
    runner = 'vimproc',
    outputter = 'quickfix',
  },
  elm = {
    runner = 'vimproc',
    command = 'elm-make',
    cmdopt = '--warn',
    exec = {
      '%c %s %o --output /tmp/vim-quickrun-elm.html',
      s'{vim.g.vimrc.open_on_gui} /tmp/vim-quickrun-elm.html',
    },
    tempfile = '%{tempname()}.elm',
  },
  idris = {
    cmdopt = '-p base -p prelude -p effects -p contrib',
  },
  happy = {
    runner = 'vimproc',
    exec = { 'happy %s', 'stack runghc %s:p:r.hs' },
    ['hook/sweep/files'] = '%S:p:r.hs',
  },
  dhall = {
    exec = { 'dhall --explain --plain %o < %s' },
  },
  dot = {
    runner = 'vimproc',
    exec = {
      'dot -T png %o %s -o %s.png',
      vim.g.vimrc.open_on_gui .. ' %s.png',
    },
    ['hook/sweep/files'] = '%S:p:r.png',
    ['outputter/error/error'] = 'quickfix',
    ['outputter/error/success'] = 'message',
  },
  python = {
    command = 'python3',
  },
}

if vim.g.vimrc.is_wsl then
  vim.g.quickrun_config.ps1 = {
    command = 'powershell.exe',
    exec = { '%c `wslpath -m %s`' },
    tempfile = '%{tempname()}.ps1',
  }
end

-- }}}
-- foldCC {{{
vim.g.foldCCtext_maxchars = 120

-- }}}
-- vim-submode {{{
vim.g.submode_timeout = 0

-- TODO: Implement submode equivalent for Neovim
-- call submode#enter_with('win_resize', 'n', '', '<C-s>w')
-- ... (other submode configurations)

-- }}}
-- aho-bakaup.vim {{{
vim.g.bakaup_backup_dir = vim.g.vimrc.backupdir
vim.g.bakaup_auto_backup = 1

-- }}}
-- neosnippet.vim {{{
vim.g.neosnippet_snippets_directory = vim.g.vimrc.vim_home .. '/neosnippets'
vim.g.neosnippet_disable_select_select_mappings = 1

-- }}}
-- vim-textobj-indent {{{
vim.g.textobj_indent_no_default_key_mappings = 1

-- }}}
-- vim-visualstar {{{
-- Do zzzv after execute visualstar
vim.g.visualstar_extra_commands = 'zzzv'

-- }}}
-- autofmt {{{
-- TODO: Implement autofmt equivalent
-- vim.opt.formatexpr = 'autofmt#japanese#formatexpr()'

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

-- let s:ghc_standard_extensions
local ghc_standard_extensions = {
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
  'typescript',
  'javascript',
  'vue',
  'typescript.tsx',
  'javascript.jsx',
}

for _, ts in ipairs(typescript_variants) do
  vim.g.ale_linters[ts] = { 'prettier', 'eslint', 'vim-lsp' }
end

vim.g.ale_scala_scalastyle_config = vim.fn.expand('$HOME/.dotfiles/scalastyle_config_default.xml')

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
-- elm-vim {{{
vim.g.elm_browser_command = vim.g.vimrc.open_on_gui
vim.g.elm_format_autosave = 1
vim.g.elm_make_output_file = '/tmp/elm-vim-output.html'
vim.g.elm_make_show_warnings = 1
vim.g.elm_setup_keybindings = 0

-- }}}
-- idris-vim {{{
vim.g.idris_vim_enable_keymappings_by_default = false

-- }}}
-- vim-highlightedyank {{{
vim.g.highlightedyank_highlight_duration = 200

-- }}}
-- vim-fmap {{{
vim.g.fmap_use_default_keymappings = false
vim.g.fmap_escape_keys = { '', '', '' }

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
-- jumpy.vim {{{
vim.g.jumpy_map = { ')', '(' }

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
-- gist.vim {{{
if vim.g.vimrc.is_wsl then
  vim.g.gist_clip_command = 'clip.exe'
end

-- }}}
-- open-browser.vim {{{
if vim.g.vimrc.is_wsl then
  -- Copied from the help of open-browser.vim
  vim.g.openbrowser_browser_commands = {
    {
      name = 'wslview',
      args = { '{browser}', '{uri}' },
    },
  }
end

-- }}}
-- previm {{{
vim.g.previm_code_language_show = 1
vim.g.previm_hard_line_break = 1

if vim.g.vimrc.is_wsl then
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
-- vim-session {{{
vim.g.session_directory = vim.g.vimrc.sessiondir
vim.g.session_autosave = 'no'
vim.g.session_autoload = 'no'

-- }}}
-- vim-write-sync {{{
vim.g.write_sync_echo_success_on_write = true

-- `['~/tmp/a', '~/tmp/b', '~/tmp/c']` for test
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

-- }}}
-- lexima.vim {{{
-- TODO: Implement lexima rules for Neovim
-- vim.call('lexima#add_rule', {char = '<', input_after = '>'})
-- ... (other lexima rules)

-- }}}
-- vital.vim {{{
-- If you want to :Vitalize,
-- do `make install-vital-vim` first,
-- then add installed vital.vim and plugins onto &rutimepath using `set rtp+=`.

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
