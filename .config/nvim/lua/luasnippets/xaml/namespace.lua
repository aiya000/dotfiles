local ls = require('luasnip')
local s = ls.snippet
local t = ls.text_node
local i = ls.insert_node
local fmt = require('luasnip.extras.fmt').fmt
local sm = ls.snippet_from_nodes
local list = require('luasnip.util.util').list
local types = require('luasnip.util.types')

local M = {
  -- CLR namespace declarations
  s('declare_xml_clr_namespace', {
    t('xmlns:'),
    i(1, 'nameSpaceName'),
    t('="clr-namespace:'),
    i(2, 'NameSpacePath'),
    t('"'),
  }),

  s('declare_xml_clr_namespace_with_assembly', {
    t('xmlns:'),
    i(1, 'nameSpaceName'),
    t('="clr-namespace:'),
    i(2, 'NameSpacePath'),
    t(';assembly='),
    i(3, 'AssemblyNameSpace'),
    t('"'),
  }),

  s('declare_xmlns_system', {
    t('xmlns:system="clr-namespace:System;assembly=mscorlib"'),
  }),

  s('declare_xml_ewpf_namespace', {
    t('xmlns:'),
    i(1, 'xctk'),
    t('="http://schemas.xceed.com/wpf/xaml/toolkit"'),
  }),
}

return M
