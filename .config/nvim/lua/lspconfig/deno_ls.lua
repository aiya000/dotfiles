vim.lsp.config('denols', {
  cmd = { 'deno', 'lsp' },
  filetypes = { 'javascript', 'javascriptreact', 'typescript', 'typescriptreact' },
  root_markers = { 'deno.json', 'deno.jsonc' },
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
