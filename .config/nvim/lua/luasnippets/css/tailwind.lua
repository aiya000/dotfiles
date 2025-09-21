-- CSS Tailwind snippets converted from neosnippet format
local ls = require("luasnip")
local s = ls.snippet
local t = ls.text_node

local M = {}

M.tailwind_vertical_center = s("tailwind_vertical_center", t("flex flex-col items-center"))

-- Convert M table to array format for LuaSnip
local snippets = {}
for _, snippet in pairs(M) do
  table.insert(snippets, snippet)
end

return { snippets = snippets, autosnippets = {} }