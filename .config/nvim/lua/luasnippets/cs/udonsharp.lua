local list = require('utils.list')
local ls = require('luasnip')
local s = ls.snippet
local t = ls.text_node
local i = ls.insert_node

-- Helper function to create snippets with multiple triggers
local function sm(triggers, nodes)
  local snippets = {}
  for _, trigger in ipairs(triggers) do
    table.insert(snippets, s(trigger, nodes))
  end
  return snippets
end

local udonsharp_snippets = {}

vim.list_extend(
  udonsharp_snippets,
  sm({ 'udon_synced_none', 'udonsharp_synced_none' }, t('[UdonSynced(UdonSyncMode.None)]'))
)

-- Import this before using below items
vim.list_extend(
  udonsharp_snippets,
  sm(
    { 'using vrc_udon_common_interfaces;', 'using_udonsharp_network_namespace' },
    t('using VRC.Udon.Common.Interfaces;')
  )
)

table.insert(udonsharp_snippets, s('udonsharp_networking_player', t('Networking.LocalPlayer')))

vim.list_extend(
  udonsharp_snippets,
  sm({ 'udonsharp_set_owner', 'udonsharp_set_game_object_owner' }, {
    t('Networking.SetOwner('),
    i(1, 'Networking.LocalPlayer'),
    t(', '),
    i(2, 'gameObject'),
    t(');'),
  })
)

return {
  snippets = udonsharp_snippets,
  autosnippets = {},
}
