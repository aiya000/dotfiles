-- HTML Bootstrap4 snippets converted from neosnippet format
local ls = require('luasnip')
local s = ls.snippet
local t = ls.text_node
local i = ls.insert_node
local fmt = require('luasnip.extras.fmt').fmt

local M = {}

-- Classes
M.bootstrap4_d_flex = s('bootstrap4_d_flex', t('d-flex'))

M.bootstrap4_flex_column = s('bootstrap4_flex_column', t('flex-column'))

M.bootstrap4_justify_content_center = s('bootstrap4_justify_content_center', t('justify-content-center'))

M.bootstrap4_justify_content_start = s('bootstrap4_justify_content_start', t('justify-content-start'))

M.bootstrap4_justify_content_end = s('bootstrap4_justify_content_end', t('justify-content-end'))

M.bootstrap4_justify_content_between = s('bootstrap4_justify_content_between', t('justify-content-between'))

M.bootstrap4_justify_content_around = s('bootstrap4_justify_content_around', t('justify-content-around'))

M.bootstrap4_align_items_center = s('bootstrap4_align_items_center', t('align-items-center'))

M.bootstrap4_align_items_start = s('bootstrap4_align_items_start', t('align-items-start'))

M.bootstrap4_align_items_end = s('bootstrap4_align_items_end', t('align-items-end'))

M.bootstrap4_align_items_between = s('bootstrap4_align_items_between', t('align-items-between'))

M.bootstrap4_align_items_around = s('bootstrap4_align_items_around', t('align-items-around'))

M.bootstrap4_align_self_center = s('bootstrap4_align_self_center', t('align-self-center'))

M.bootstrap4_align_self_start = s('bootstrap4_align_self_start', t('align-self-start'))

M.bootstrap4_align_self_end = s('bootstrap4_align_self_end', t('align-self-end'))

M.bootstrap4_align_self_between = s('bootstrap4_align_self_between', t('align-self-between'))

M.bootstrap4_align_self_around = s('bootstrap4_align_self_around', t('align-self-around'))

M.img_fluid = s('img_fluid', t('img-fluid'))

-- Templates
M.bootstrap4_flex_vertical_center = s(
  'bootstrap4_flex_vertical_center',
  fmt([[<div class="d-flex flex-row justify-content-center">{}</div>]], {
    i(1, 'here'),
  })
)

M.bootstrap4_div_container = s(
  'bootstrap4_div_container',
  fmt([[<div class="container">{}</div>]], {
    i(1, 'here'),
  })
)

-- Convert M table to array format for LuaSnip
local snippets = {}
for _, snippet in pairs(M) do
  table.insert(snippets, snippet)
end

return { snippets = snippets, autosnippets = {} }
