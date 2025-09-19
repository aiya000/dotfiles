local list = require('utils.list')

-- LuaSnip from_lua loader用の正しい形式
return {
  snippets = list.concat({
  require('luasnippets.scala.scala'),
  autosnippets = {}
},
  require('luasnippets.scala.scaladoc'),
  require('luasnippets.scala.scalastyle'),
  require('luasnippets.scala.scalatest'),
  require('luasnippets.scala.wartremover'),
})