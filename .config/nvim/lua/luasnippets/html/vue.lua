-- HTML Vue.js snippets converted from neosnippet format
local ls = require("luasnip")
local s = ls.snippet
local t = ls.text_node
local i = ls.insert_node
local fmt = require("luasnip.extras.fmt").fmt

local M = {}

-- Function to create multiple aliases for a snippet
local function sm(trigger, aliases, snippet_def)
  local result = {[trigger] = snippet_def}
  for _, alias in ipairs(aliases) do
    result[alias] = snippet_def
  end
  return result
end

-- Vue.js directives
local v_if_snippets = sm("v_if", {"vif", "v-if"}, s("v_if",
  fmt([[v-if="{}"]], {
    i(1, "cond")
  })
))
for k, v in pairs(v_if_snippets) do
  M[k] = v
end

local v_else_if_snippets = sm("v_else_if", {"velseif", "v-else-if"}, s("v_else_if",
  fmt([[v-else-if="{}"]], {
    i(1, "cond")
  })
))
for k, v in pairs(v_else_if_snippets) do
  M[k] = v
end

local v_else_snippets = sm("v_else", {"velse", "v-else"}, s("v_else", t("v-else")))
for k, v in pairs(v_else_snippets) do
  M[k] = v
end

local v_for_snippets = sm("v_for", {"vfor", "v-for"}, s("v_for",
  fmt([[v-for="{} in {}"]], {
    i(1, "x"),
    i(2, "xs")
  })
))
for k, v in pairs(v_for_snippets) do
  M[k] = v
end

local v_model_snippets = sm("v_model", {"vmodel", "v-model"}, s("v_model",
  fmt([[v-model="{}"]], {
    i(1, "data_property")
  })
))
for k, v in pairs(v_model_snippets) do
  M[k] = v
end

-- Convert M table to array format for LuaSnip
local snippets = {}
for _, snippet in pairs(M) do
  table.insert(snippets, snippet)
end

return { snippets = snippets, autosnippets = {} }