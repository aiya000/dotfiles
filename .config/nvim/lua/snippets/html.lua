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
  require('snippets.html.bootstrap4'),
  require('snippets.html.attr'),
  require('snippets.html.vue'),
  require('snippets.html.html'),
  require('snippets.html.nativescript'),
  require('snippets.html.template')
)