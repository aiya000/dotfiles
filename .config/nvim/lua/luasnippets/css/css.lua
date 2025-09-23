-- CSS snippets converted from neosnippet format
local ls = require('luasnip')
local s = ls.snippet
local t = ls.text_node
local i = ls.insert_node
local fmt = require('luasnip.extras.fmt').fmt

local M = {}

-- Function to create multiple aliases for a snippet
local function sm(trigger, aliases, snippet_def)
  local result = { [trigger] = snippet_def }
  for _, alias in ipairs(aliases) do
    result[alias] = snippet_def
  end
  return result
end

-- Syntaxes
local class_snippets = sm(
  'class',
  { 'cla', 'cl', 'scss_class' },
  s(
    'class',
    fmt([[.{} {{{}}}]], {
      i(1, 'name'),
      i(2, '#:here'),
    })
  )
)
for k, v in pairs(class_snippets) do
  M[k] = v
end

M.id = s(
  'id',
  fmt([[#{} {{{}}}]], {
    i(1, 'name'),
    i(2, '#:here'),
  })
)

M.tag = s(
  'tag',
  fmt([[{} {{{}}}]], {
    i(1, 'name'),
    i(2, '#:here'),
  })
)

M.regex_wildcard = s(
  'regex_wildcard',
  fmt([[[{}*='{}'] {{{}}}]], {
    i(1, 'class'),
    i(2, 'heading'),
    i(3, '#:here'),
  })
)

M.media_query = s(
  'media_query',
  fmt([[@media screen and (max-width: {}) {{{}}}]], {
    i(1, '1024px'),
    i(2, '#:here'),
  })
)

-- Templates
M.media_query_tablet = s(
  'media_query_tablet',
  fmt([[@media screen and (max-width: 1024px) {{{}}}]], {
    i(1, '#:here'),
  })
)

M.media_query_mobile = s(
  'media_query_mobile',
  fmt([[@media screen and (max-width: 575px) {{{}}}]], {
    i(1, '#:here'),
  })
)

M.display_flex = s('display_flex', t('display: flex;'))

M.flex_direction = s(
  'flex_direction',
  fmt([[flex-direction: {};]], {
    i(1, 'column'),
  })
)

M.justify_content = s(
  'justify_content',
  fmt([[justify-content: {};]], {
    i(1, 'center'),
  })
)

M.align_items = s(
  'align_items',
  fmt([[align-items: {};]], {
    i(1, 'center'),
  })
)

M.align_self = s(
  'align_self',
  fmt([[align-self: {};]], {
    i(1, 'center'),
  })
)

-- Others
local comment_snippets = sm(
  'comment',
  { 'com' },
  s(
    'comment',
    fmt([[/* {} */]], {
      i(1, 'here'),
    })
  )
)
for k, v in pairs(comment_snippets) do
  M[k] = v
end

-- Convert M table to array format for LuaSnip
local snippets = {}
for _, snippet in pairs(M) do
  table.insert(snippets, snippet)
end

return { snippets = snippets, autosnippets = {} }
