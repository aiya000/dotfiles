local fmt = require('luasnip.extras.fmt').fmt
local list = require('utils.list')
local ls = require('luasnip')
local utils = require('utils.luasnip')

local sm = utils.sm
local s = ls.snippet
local i = ls.insert_node
local t = ls.text_node

local xml_snippets = {}

-- XML header
table.insert(xml_snippets, s('xml_format', t('<?xml version="1.0" encoding="utf-8"?>')))

-- Basic XML constructs
vim.list_extend(xml_snippets, sm({ 'comment', 'com' }, fmt('<!-- {} -->', {
  i(1, ''),
})))

vim.list_extend(xml_snippets, sm({ 'single', 'sin' }, fmt('<{} />', {
  i(1, 'name'),
})))

vim.list_extend(xml_snippets, sm({ 'surround', 'sur' }, fmt('<{}>{}<//{}>', {
  i(1, 'name'),
  i(2, 'here'),
  i(3, ''),
})))

return { snippets = xml_snippets, autosnippets = {} }