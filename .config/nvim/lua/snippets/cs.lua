local list = require('utils.list')

-- Import all C# snippet modules
local cs_snippets = require('snippets.cs.cs')
local method_snippets = require('snippets.cs.method')
local attr_snippets = require('snippets.cs.attr')
local pragma_snippets = require('snippets.cs.pragma')
local unity_snippets = require('snippets.cs.unity')
local udonsharp_snippets = require('snippets.cs.udonsharp')
local region_snippets = require('snippets.cs.region')
local template_snippets = require('snippets.cs.template')
local import_snippets = require('snippets.cs.import')

-- Concatenate all snippets into a single table
return list.concat({
  cs_snippets,
  method_snippets,
  attr_snippets,
  pragma_snippets,
  unity_snippets,
  udonsharp_snippets,
  region_snippets,
  template_snippets,
  import_snippets
})