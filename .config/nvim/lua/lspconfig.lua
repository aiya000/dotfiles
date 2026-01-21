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

local augroup = vim.api.nvim_create_augroup('InitLuaLspconfig', { clear = true })

vim.api.nvim_create_autocmd('FileType', {
  group = augroup,
  pattern = { 'lua' },
  once = true,
  callback = function()
    require('lspconfig.lua_ls')
  end,
})

vim.api.nvim_create_autocmd('FileType', {
  group = augroup,
  pattern = { 'javascript', 'javascriptreact', 'typescript', 'typescriptreact', 'vue' },
  once = true,
  callback = function()
    require('lspconfig.ts_ls')
  end,
})

vim.api.nvim_create_autocmd('FileType', {
  group = augroup,
  pattern = { 'vue' },
  once = true,
  callback = function()
    require('lspconfig.vue_ls')
  end,
})
