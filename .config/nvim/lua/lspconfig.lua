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

---@param filetypes string[]
---@param module_name string
---@param p? fun(): boolean
local function require_lsp_when_filetype(filetypes, module_name, p)
  vim.api.nvim_create_autocmd('FileType', {
    group = augroup,
    pattern = filetypes,
    once = true,
    callback = function()
      if p == nil or p() then
        require(module_name)
      end
    end,
  })
end

require_lsp_when_filetype({ 'lua' }, 'lspconfig.lua_ls')
require_lsp_when_filetype(
  { 'javascript', 'javascriptreact', 'typescript', 'typescriptreact', 'vue' },
  'lspconfig.ts_ls'
)
require_lsp_when_filetype({ 'vue' }, 'lspconfig.vue_ls')
