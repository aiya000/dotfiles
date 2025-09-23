-- HTML Template snippets converted from neosnippet format
local ls = require('luasnip')
local s = ls.snippet
local t = ls.text_node
local i = ls.insert_node
local fmt = require('luasnip.extras.fmt').fmt

local M = {}

M.template = s(
  'template',
  fmt(
    [[<!DOCTYPE html>
<html lang="ja">

<head>
    <meta charset="UTF-8">
    <title>{}</title>
    <meta name="viewport" content="width=device-width, initial-scale=1, maximum-scale=1">
</head>

<body>
    {}
</body>
</html>]],
    {
      i(1, 'name'),
      i(2, 'TARGET'),
    }
  )
)

-- Convert M table to array format for LuaSnip
local snippets = {}
for _, snippet in pairs(M) do
  table.insert(snippets, snippet)
end

return { snippets = snippets, autosnippets = {} }
