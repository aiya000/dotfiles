local list = require('utils.list')

-- LuaSnip from_lua loader用の正しい形式
return {
  snippets = list.concat(
    require("luasnippets.fxml.attr"),
    require("luasnippets.fxml.fxml"),
    require("luasnippets.fxml.template")
  ),
  autosnippets = {}
}