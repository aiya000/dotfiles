local list = require('utils.list')

-- Import all C++ snippet modules
local cpp_snippets = require('snippets.cpp.cpp')
local doxygen_snippets = require('snippets.cpp.doxygen')
local gtest_snippets = require('snippets.cpp.gtest')

-- Concatenate all snippets into a single table
return list.concat({
  cpp_snippets,
  doxygen_snippets,
  gtest_snippets
})