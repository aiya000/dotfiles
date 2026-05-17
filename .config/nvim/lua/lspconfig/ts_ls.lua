-- Masonでインストールされた@vue/typescript-pluginのパス
local vue_typescript_plugin_path = vim.fn.expand(
  '~/.local/share/nvim/mason/packages/vue-language-server/node_modules/@vue/typescript-plugin'
)

if not vim.uv.fs_stat(vue_typescript_plugin_path) then
  vim.notify(('[ts_ls] @vue/typescript-plugin not found: %s'):format(vue_typescript_plugin_path), vim.log.levels.WARN)
end

vim.lsp.config('ts_ls', {
  name = 'ts_ls',
  cmd = { 'typescript-language-server', '--stdio' },
  filetypes = {
    'javascript',
    'javascriptreact',
    'javascript.jsx',
    'typescript',
    'typescriptreact',
    'typescript.tsx',
    'vue',
  },
  root_dir = function(bufnr, on_dir)
    if vim.fs.root(bufnr, { 'deno.json', 'deno.jsonc' }) then
      return
    end
    local root = vim.fs.root(bufnr, { 'tsconfig.json', 'jsconfig.json', 'package.json', '.git' })
      or vim.fs.dirname(vim.api.nvim_buf_get_name(bufnr))
    on_dir(root)
  end,
  init_options = {
    hostInfo = 'neovim',
    plugins = {
      {
        name = '@vue/typescript-plugin',
        location = vue_typescript_plugin_path,
        languages = { 'vue' },
      },
    },
  },
  settings = {
    typescript = {
      inlayHints = {
        includeInlayParameterNameHints = 'all',
        includeInlayParameterNameHintsWhenArgumentMatchesName = false,
        includeInlayFunctionParameterTypeHints = true,
        includeInlayVariableTypeHints = true,
        includeInlayPropertyDeclarationTypeHints = true,
        includeInlayFunctionLikeReturnTypeHints = true,
        includeInlayEnumMemberValueHints = true,
      },
    },
    javascript = {
      inlayHints = {
        includeInlayParameterNameHints = 'all',
        includeInlayParameterNameHintsWhenArgumentMatchesName = false,
        includeInlayFunctionParameterTypeHints = true,
        includeInlayVariableTypeHints = true,
        includeInlayPropertyDeclarationTypeHints = true,
        includeInlayFunctionLikeReturnTypeHints = true,
        includeInlayEnumMemberValueHints = true,
      },
    },
  },
})

vim.lsp.enable('ts_ls')
