local list = require('utils.list')

return list.concat(
  require('snippets.lua.lua'),
  require('snippets.lua.lsp'),
  require('snippets.lua.luaCATS'),
  require('snippets.lua.neovim'),
  require('snippets.lua.templates')
)