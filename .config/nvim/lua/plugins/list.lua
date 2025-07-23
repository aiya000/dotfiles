-- Plugin specifications for lazy.nvim

return {
  -- Core dependencies
  {
    'vim-denops/denops.vim',
  },

  -- Text objects
  {
    'kana/vim-textobj-user',
  },
  {
    'kana/vim-textobj-indent',
    dependencies = { 'vim-textobj-user' },
  },
  {
    'mattn/vim-textobj-between',
    dependencies = { 'vim-textobj-user' },
  },
  {
    'aiya000/vim-textobj-jabraces',
    dependencies = { 'vim-textobj-user' },
  },

  -- Operators
  {
    'kana/vim-operator-user',
  },
  {
    'rhysd/vim-operator-surround',
    dependencies = { 'vim-operator-user' },
  },
  {
    'tyru/operator-camelize.vim',
    dependencies = { 'vim-operator-user' },
  },

  -- Motion and search
  {
    'haya14busa/vim-visualstar',
  },
  {
    'haya14busa/incsearch.vim',
  },
  {
    'osyo-manga/vim-anzu',
  },
  {
    'lambdalisue/kensaku-search.vim',
  },
  {
    'aiya000/vim-fmap',
  },

  -- File management
  {
    'justinmk/vim-dirvish',
  },
  {
    'lambdalisue/fern.vim',
  },

  -- Completion and snippets
  {
    'Shougo/ddc.vim',
    dependencies = { 'denops.vim' },
  },
  {
    'Shougo/ddc-ui-native',
    dependencies = { 'ddc.vim' },
  },
  {
    'Shougo/ddc-source-around',
    dependencies = { 'ddc.vim' },
  },
  {
    'Shougo/ddc-source-file',
    dependencies = { 'ddc.vim' },
  },
  {
    'Shougo/ddc-source-buffer',
    dependencies = { 'ddc.vim' },
  },
  {
    'shun/ddc-source-vim-lsp',
    dependencies = { 'ddc.vim' },
  },
  {
    'Shougo/ddc-matcher_head',
    dependencies = { 'ddc.vim' },
  },
  {
    'Shougo/ddc-matcher_fuzzy',
    dependencies = { 'ddc.vim' },
  },
  {
    'Shougo/ddc-sorter_rank',
    dependencies = { 'ddc.vim' },
  },
  {
    'Shougo/ddc-sorter_fuzzy',
    dependencies = { 'ddc.vim' },
  },
  {
    'Shougo/ddc-converter_fuzzy',
    dependencies = { 'ddc.vim' },
  },
  {
    'Shougo/neosnippet.vim',
  },
  {
    'Shougo/ddc-source-neosnippet',
    dependencies = { 'ddc.vim', 'neosnippet.vim' },
  },

  -- Fuzzy finder
  {
    'Shougo/ddu.vim',
    dependencies = { 'denops.vim' },
  },
  {
    'Shougo/ddu-ui-ff',
    dependencies = { 'ddu.vim' },
  },
  {
    'Shougo/ddu-source-file_rec',
    dependencies = { 'ddu.vim' },
  },
  {
    'Shougo/ddu-source-line',
    dependencies = { 'ddu.vim' },
  },
  {
    'Shougo/ddu-source-buffer',
    dependencies = { 'ddu.vim' },
  },
  {
    'Shougo/ddu-source-file_old',
    dependencies = { 'ddu.vim' },
  },
  {
    'Shougo/ddu-source-help',
    dependencies = { 'ddu.vim' },
  },
  {
    'uga-rosa/ddu-source-lsp',
    dependencies = { 'ddu.vim' },
  },
  {
    'Shougo/ddu-filter-matcher_substring',
    dependencies = { 'ddu.vim' },
  },
  {
    'Shougo/ddu-filter-matcher_regex',
    dependencies = { 'ddu.vim' },
  },
  {
    'Shougo/ddu-kind-file',
    dependencies = { 'ddu.vim' },
  },

  -- LSP
  {
    'prabirshrestha/vim-lsp',
  },
  {
    'mattn/vim-lsp-settings',
    dependencies = { 'vim-lsp' },
  },

  -- Linting and formatting
  {
    'dense-analysis/ale',
  },

  -- Git
  {
    'lambdalisue/gin.vim',
    dependencies = { 'denops.vim' },
  },

  -- UI enhancements
  {
    'nathanaelkane/vim-indent-guides',
  },
  {
    'itchyny/vim-cursorword',
  },
  {
    'machakann/vim-highlightedyank',
  },
  {
    'luochen1990/rainbow',
  },

  -- Utilities
  {
    'thinca/vim-quickrun',
  },
  {
    'cohama/lexima.vim',
  },
  {
    'tpope/vim-repeat',
  },
  {
    'LeafCage/yankround.vim',
  },
  {
    'mbbill/undotree',
  },
  {
    'tyru/open-browser.vim',
  },
  {
    'mattn/gist-vim',
    dependencies = { 'mattn/webapi-vim' },
  },
  {
    'mattn/webapi-vim',
  },
  {
    'previm/previm',
    dependencies = { 'open-browser.vim' },
  },
  {
    'aiya000/vim-quickrepl',
  },
  {
    'aiya000/vim-ghcid-quickfix',
  },
  {
    'aiya000/vim-webpage',
  },
  {
    'aiya000/translate.vim',
  },
  {
    'aiya000/jumpy.vim',
  },
  {
    'aiya000/vim-session',
  },
  {
    'aiya000/vim-write-sync',
  },
  {
    'aiya000/vim-scratch-buffer',
  },
  {
    'aiya000/deepl.vim',
    dependencies = { 'denops.vim' },
  },
  {
    'github/copilot.vim',
  },
  {
    'skywind3000/asyncrun.vim',
  },
  {
    'osyo-manga/vim-precious',
  },
  {
    'Shougo/context_filetype.vim',
  },
  {
    'aiya000/sync-term-cwd.vim',
  },
  {
    'kana/vim-submode',
  },
  {
    't9md/vim-textobj-from_regexp',
    dependencies = { 'vim-textobj-user' },
  },
  {
    'junegunn/vim-easy-align',
  },
  {
    'aiya000/aho-bakaup.vim',
  },
  {
    'LeafCage/foldCC',
  },
  {
    'ujihisa/vital.vim',
  },
  {
    'aiya000/quickpeek.vim',
  },

  -- Language specific
  {
    'elm-tooling/elm-vim',
    ft = 'elm',
  },
  {
    'idris-hackers/idris-vim',
    ft = 'idris',
  },

  -- Colorscheme
  {
    'aiya000/vim-colors-lucariox',
    priority = 1000,
    config = function()
      -- TODO: Set colorscheme when available
      -- vim.cmd("colorscheme lucariox")
    end,
  },
}
