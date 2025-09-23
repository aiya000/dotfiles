local fmt = require('luasnip.extras.fmt').fmt
local list = require('utils.list')
local ls = require('luasnip')
local utils = require('utils.luasnip')

local sm = utils.sm
local s = ls.snippet
local i = ls.insert_node
local t = ls.text_node

local csproj_snippets = {}

-- C# project file snippets
table.insert(
  csproj_snippets,
  s(
    'DependentUpon_surround',
    fmt('<DependentUpon>{}</DependentUpon>', {
      i(1, 'ParentFileName'),
    })
  )
)

table.insert(
  csproj_snippets,
  s(
    'compile_template',
    fmt('<Compile Include="{}"/>', {
      i(1, 'FileName'),
    })
  )
)

table.insert(
  csproj_snippets,
  s(
    'compile_template_surround',
    fmt('<Compile Include="{}">{}</Compile>', {
      i(1, 'FileName'),
      i(2, ''),
    })
  )
)

return { snippets = csproj_snippets, autosnippets = {} }
