local fmt = require('luasnip.extras.fmt').fmt
local list = require('utils.list')
local ls = require('luasnip')
local utils = require('utils.luasnip')

local sm = utils.sm
local s = ls.snippet
local i = ls.insert_node
local t = ls.text_node

return {
  -- C# project file snippets
  s('DependentUpon_surround', fmt('<DependentUpon>{}</DependentUpon>', {
    i(1, 'ParentFileName'),
  })),

  s('compile_template', fmt('<Compile Include="{}"/>', {
    i(1, 'FileName'),
  })),

  s('compile_template_surround', fmt('<Compile Include="{}">{}</Compile>', {
    i(1, 'FileName'),
    i(2, ''),
  })),
}