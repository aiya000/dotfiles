-- HTML Attributes snippets converted from neosnippet format
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

-- HTML attributes
local html_attribute_snippets = sm(
  'html_attribute',
  { 'attribute', 'attr', 'att' },
  s(
    'html_attribute',
    fmt([[{}="{}"]], {
      i(1, 'name'),
      i(2, 'here'),
    })
  )
)
for k, v in pairs(html_attribute_snippets) do
  M[k] = v
end

M.colspan = s(
  'colspan',
  fmt([[colspan="{}"]], {
    i(1, 'here'),
  })
)

local class_snippets = sm(
  'class',
  { 'cl', 'cla', 'html_class' },
  s(
    'class',
    fmt([[class="{}"]], {
      i(1, 'name'),
    })
  )
)
for k, v in pairs(class_snippets) do
  M[k] = v
end

M.id = s(
  'id',
  fmt([[id="{}"]], {
    i(1, 'id'),
  })
)

M.height = s(
  'height',
  fmt([[height="{}"]], {
    i(1, '64'),
  })
)

M.width = s(
  'width',
  fmt([[width="{}"]], {
    i(1, '64'),
  })
)

M.name = s(
  'name',
  fmt([[name="{}"]], {
    i(1, 'here'),
  })
)

M.type = s(
  'type',
  fmt([[type="{}"]], {
    i(1, 'here'),
  })
)

M.rel = s(
  'rel',
  fmt([[rel="{}"]], {
    i(1, 'here'),
  })
)

M.alt = s(
  'alt',
  fmt([[alt="{}"]], {
    i(1, 'here'),
  })
)

M.href = s(
  'href',
  fmt([[href="{}"]], {
    i(1, 'here'),
  })
)

M.style = s(
  'style',
  fmt([[style="{}"]], {
    i(1, 'here'),
  })
)

-- Templates
local rel_noopener_snippets = sm('rel_noopener', { 'target_blank_secure_attr' }, s('rel_noopener', t('rel="noopener"')))
for k, v in pairs(rel_noopener_snippets) do
  M[k] = v
end

-- Convert M table to array format for LuaSnip
local snippets = {}
for _, snippet in pairs(M) do
  table.insert(snippets, snippet)
end

return { snippets = snippets, autosnippets = {} }
