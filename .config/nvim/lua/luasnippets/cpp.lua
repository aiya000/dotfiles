local list = require('utils.list')

-- Import all C++ snippet modules
local cpp_snippets = require('luasnippets.cpp.cpp')
local doxygen_snippets = require('luasnippets.cpp.doxygen')
local gtest_snippets = require('luasnippets.cpp.gtest')

-- Concatenate all snippets into a single table
-- LuaSnip from_lua loader用の正しい形式
return {
  snippets = list.concat(
    cpp_snippets.snippets,
    doxygen_snippets.snippets,
    gtest_snippets.snippets
  ),
  autosnippets = {}
}