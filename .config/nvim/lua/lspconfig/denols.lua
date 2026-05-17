vim.lsp.config('denols', {
  name = 'denols',
  cmd = { 'deno', 'lsp' },
  filetypes = { 'javascript', 'javascriptreact', 'typescript', 'typescriptreact' },
  root_markers = { 'deno.json', 'deno.jsonc' },
  workspace_required = true,
  settings = {
    deno = {
      enable = true,
      suggest = {
        imports = { hosts = { ['https://deno.land'] = true } },
      },
    },
  },
})

vim.lsp.enable('denols')
