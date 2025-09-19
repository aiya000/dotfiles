local list = require('utils.list')

-- Import all Markdown snippet modules
local markdown_snippets = require('luasnippets.markdown.markdown')
local blocks_snippets = require('luasnippets.markdown.blocks')
local kaomoji_snippets = require('luasnippets.markdown.kaomoji')
local html_snippets = require('luasnippets.markdown.html')
local emoji_snippets = require('luasnippets.markdown.emoji')
local subjects_snippets = require('luasnippets.markdown.subjects')
local markdown_pp_snippets = require('luasnippets.markdown.markdown-pp')
local html_attr_snippets = require('luasnippets.markdown.html-attr')
local zenn_snippets = require('luasnippets.markdown.zenn')

-- Concatenate all snippets into a single table
-- LuaSnip from_lua loader用の正しい形式
return {
  snippets = list.concat({
  markdown_snippets,
  blocks_snippets,
  kaomoji_snippets,
  html_snippets,
  emoji_snippets,
  subjects_snippets,
  markdown_pp_snippets,
  html_attr_snippets,
  zenn_snippets
}),
  autosnippets = {}
}