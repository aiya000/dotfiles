local ls = require("luasnip")
local s = ls.snippet
local t = ls.text_node
local i = ls.insert_node
local fmt = require("luasnip.extras.fmt").fmt
local sm = ls.snippet_from_nodes
local list = require("luasnip.util.util").list
local types = require("luasnip.util.types")

local M = {
  -- Standard XML namespaces
  s('xmlns', {
    t('xmlns="http://schemas.microsoft.com/winfx/2006/xaml/presentation"')
  }),

  s('xmlns_x', {
    t('xmlns:x="http://schemas.microsoft.com/winfx/2006/xaml"')
  }),

  s('xmlns_d', {
    t('xmlns:d="http://schemas.microsoft.com/expression/blend/2008"')
  }),

  s('xmlns_mc', {
    t('xmlns:mc="http://schemas.openxmlformats.org/markup-compatibility/2006"')
  }),

  s('xmlns_local', {
    t('xmlns:local="clr-namespace:'), i(1, 'CSharpCurrentNameSpace'), t('"')
  }),

  s({'xmlns_i', 'xmlns_interactivity'}, {
    t('xmlns:i="clr-namespace:System.Windows.Interactivity;assembly=System.Windows.Interactivity"')
  }),

  s('xmlns_input', {
    t('xmlns:input="http://schemas.microsoft.com/netfx/2007/xaml/presentation"')
  }),
}

return M