local function get_tsdk(root_dir)
  -- プロジェクトのnode_modules内のtypescriptを探す
  local local_tsdk = root_dir .. '/node_modules/typescript/lib'
  if vim.uv.fs_stat(local_tsdk) then
    return local_tsdk
  end

  -- グローバルのtypescriptを探す（bunやnpmでグローバルインストールされている場合）
  local global_tsdk = vim.fn.expand('~/.bun/install/global/node_modules/typescript/lib')
  if vim.uv.fs_stat(global_tsdk) then
    return global_tsdk
  end

  return nil
end

vim.lsp.config('vue_ls', {
  cmd = { 'vue-language-server', '--stdio' },
  filetypes = { 'vue' },
  root_markers = { 'package.json', 'vue.config.js', 'vue.config.ts', '.git' },
  init_options = {
    typescript = {
      tsdk = '',
    },
    vue = {
      hybridMode = true,
    },
  },
  before_init = function(params, config)
    local root_dir = params.rootPath or vim.fn.getcwd()
    local tsdk = get_tsdk(root_dir)
    if tsdk == nil then
      vim.notify('[vue_ls] tsdk not found. TypeScript features may not work.', vim.log.levels.WARN)
    end
    config.init_options.typescript.tsdk = tsdk
  end,
  on_attach = function(client)
    -- hybridModeではts_lsがhoverを担当するので、vue_lsのhoverを無効化
    client.server_capabilities.hoverProvider = false
  end,
  settings = {
    typescript = {
      inlayHints = {
        enumMemberValues = { enabled = true },
        functionLikeReturnTypes = { enabled = true },
        parameterNames = { enabled = 'all' },
        parameterTypes = { enabled = true },
        propertyDeclarationTypes = { enabled = true },
        variableTypes = { enabled = true },
      },
    },
  },
})

vim.lsp.enable('vue_ls')
