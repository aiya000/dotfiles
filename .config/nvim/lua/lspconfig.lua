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

require('lspconfig.lua_ls')
require('lspconfig.ts_ls')
