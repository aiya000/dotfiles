-- Combine all CSS snippet modules
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
  require('luasnippets.css.css'),
  require('luasnippets.css.tailwind')
)