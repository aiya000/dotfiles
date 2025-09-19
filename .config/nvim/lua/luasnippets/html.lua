-- Combine all HTML snippet modules
local function combine_snippets(...)
  local result = {}
  for _, module in ipairs({...}) do
    for key, snippet in pairs(module) do
      result[key] = snippet
    end
  end
  return result
end

return combine_snippets(
  require('luasnippets.html.bootstrap4'),
  require('luasnippets.html.attr'),
  require('luasnippets.html.vue'),
  require('luasnippets.html.html'),
  require('luasnippets.html.nativescript'),
  require('luasnippets.html.template')
)