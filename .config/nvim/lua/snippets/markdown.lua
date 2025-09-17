local list = require('utils.list')

-- Import all Markdown snippet modules
local markdown_snippets = require('snippets.markdown.markdown')
local blocks_snippets = require('snippets.markdown.blocks')
local kaomoji_snippets = require('snippets.markdown.kaomoji')
local html_snippets = require('snippets.markdown.html')
local emoji_snippets = require('snippets.markdown.emoji')
local subjects_snippets = require('snippets.markdown.subjects')
local markdown_pp_snippets = require('snippets.markdown.markdown-pp')
local html_attr_snippets = require('snippets.markdown.html-attr')
local zenn_snippets = require('snippets.markdown.zenn')

-- Concatenate all snippets into a single table
return list.concat({
  markdown_snippets,
  blocks_snippets,
  kaomoji_snippets,
  html_snippets,
  emoji_snippets,
  subjects_snippets,
  markdown_pp_snippets,
  html_attr_snippets,
  zenn_snippets
})