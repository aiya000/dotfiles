local list = require('utils.list')

return {
  snippets = list.concat(require('luasnippets.typescript_tsx.typescript_tsx').snippets),
  autosnippets = {},
}
