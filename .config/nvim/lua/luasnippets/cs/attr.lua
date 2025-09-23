local list = require('utils.list')
local ls = require('luasnip')
local s = ls.snippet
local t = ls.text_node
local i = ls.insert_node
local fmt = require('luasnip.extras.fmt').fmt

local attr_snippets = {
  s(
    'InternalsVisibleToAttribute',
    fmt(
      [[
#pragma warning disable 0657
[assembly: InternalsVisibleTo("{}")]
#pragma warning restore 0657{}]],
      {
        i(1, '#:FriendName'),
        i(2, ''),
      }
    )
  ),

  s('DebuggerDisplayAttribute', {
    t('[DebuggerDisplayAttribute("'),
    i(1, '#:Name'),
    t(' = {'),
    i(2, '#:Member'),
    t('}")]'),
    i(3, ''),
  }),

  s('TestClassAttribute', {
    t('[TestClass]'),
    i(1, ''),
  }),

  s('TestMethodAttribute', {
    t('[TestMethod]'),
    i(1, ''),
  }),

  s('TestInitializeAttribute', {
    t('[TestInitialize]'),
    i(1, ''),
  }),

  s('TestCleanupAttribute', {
    t('[TestCleanup]'),
    i(1, ''),
  }),
}

return {
  snippets = attr_snippets,
  autosnippets = {},
}
