-- Load all neosnippet meta snippets
local neosnippet_snippets = require("snippets.neosnippet.neosnippet")
local c_like_snippets = require("snippets.neosnippet.c_like")
local html_like_snippets = require("snippets.neosnippet.html_like")
local javascript_like_snippets = require("snippets.neosnippet.javascript_like")
local haskell_snippets = require("snippets.neosnippet.haskell")
local idris_snippets = require("snippets.neosnippet.idris")

local all_snippets = {}

-- Combine all snippets
for _, snippet in ipairs(neosnippet_snippets) do
  table.insert(all_snippets, snippet)
end

for _, snippet in ipairs(c_like_snippets) do
  table.insert(all_snippets, snippet)
end

for _, snippet in ipairs(html_like_snippets) do
  table.insert(all_snippets, snippet)
end

for _, snippet in ipairs(javascript_like_snippets) do
  table.insert(all_snippets, snippet)
end

for _, snippet in ipairs(haskell_snippets) do
  table.insert(all_snippets, snippet)
end

for _, snippet in ipairs(idris_snippets) do
  table.insert(all_snippets, snippet)
end

return all_snippets