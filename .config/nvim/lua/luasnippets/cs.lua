local list = require('utils.list')

-- Import all C# snippet modules
local cs_snippets = require('luasnippets.cs.cs')
local method_snippets = require('luasnippets.cs.method')
local attr_snippets = require('luasnippets.cs.attr')
local pragma_snippets = require('luasnippets.cs.pragma')
local unity_snippets = require('luasnippets.cs.unity')
local udonsharp_snippets = require('luasnippets.cs.udonsharp')
local region_snippets = require('luasnippets.cs.region')
local template_snippets = require('luasnippets.cs.template')
local import_snippets = require('luasnippets.cs.import')

-- Concatenate all snippets into a single table
-- LuaSnip from_lua loader用の正しい形式
return {
  snippets = list.concat({
  cs_snippets,
  method_snippets,
  attr_snippets,
  pragma_snippets,
  unity_snippets,
  udonsharp_snippets,
  region_snippets,
  template_snippets,
  import_snippets
}),
  autosnippets = {}
}