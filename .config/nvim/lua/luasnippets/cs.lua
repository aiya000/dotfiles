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
  snippets = list.concat(
    cs_snippets.snippets,
    method_snippets.snippets,
    attr_snippets.snippets,
    pragma_snippets.snippets,
    unity_snippets.snippets,
    udonsharp_snippets.snippets,
    region_snippets.snippets,
    template_snippets.snippets,
    import_snippets.snippets
  ),
  autosnippets = {},
}
