local list = require('utils.list')
local ls = require('luasnip')
local s = ls.snippet
local t = ls.text_node

-- Helper function to create snippets with multiple triggers
local function sm(triggers, nodes)
  local snippets = {}
  for _, trigger in ipairs(triggers) do
    table.insert(snippets, s(trigger, nodes))
  end
  return snippets
end

-- Convert to proper array structure for LuaSnip compatibility
local kaomoji_snippets = {}
table.insert(kaomoji_snippets, s('kaomoji_ehehe', t('(,,>᎑<,,)')))
table.insert(kaomoji_snippets, s('kaomoji_ehehe1', t('(,,･ ･,,)')))
table.insert(kaomoji_snippets, s('kaomoji_ehehe2', t('(๑>◡<๑)')))
table.insert(kaomoji_snippets, s('kaomoji_ehehe3', t('(>᎑<`๑)')))
table.insert(kaomoji_snippets, s('kaomoji_ehehe4', t('(ˊo̴̶̷̤ ᴗ o̴̶̷̤ˋ)')))
table.insert(kaomoji_snippets, s('kaomoji_ehehe5', t('(ू•‧̫•ू⑅)')))
table.insert(kaomoji_snippets, s('kaomoji_wai', t('⸜(* ॑꒳ ॑* )⸝')))
vim.list_extend(kaomoji_snippets, sm({ 'kaomoji_wai1', 'kaomoji_chomado_san' }, t('(((o(*ﾟ▽ﾟ*)o)))')))
table.insert(kaomoji_snippets, s('kaomoji_no_emote', t("(*'-')")))
table.insert(kaomoji_snippets, s('kaomoji_hi', t("(*'-')ﾉ~")))
table.insert(kaomoji_snippets, s('kaomoji_hi1', t('(✿╹◡╹)ﾉ')))
table.insert(kaomoji_snippets, s('kaomoji_hi2', t('ㄟ( ･ө･ )ㄏ')))
table.insert(kaomoji_snippets, s('kaomoji_hamster', t('(๑╹ᆺ╹)')))
table.insert(kaomoji_snippets, s('kaomoji_zombi', t('ԅ(¯﹃¯ԅ))))')))
table.insert(kaomoji_snippets, s('kaomoji_een', t('(৹˃ᗝ˂৹)')))
table.insert(kaomoji_snippets, s('kaomoji_een1', t('(;>_<;)')))
table.insert(kaomoji_snippets, s('kaomoji_een2', t('(ó﹏ò｡)')))
table.insert(kaomoji_snippets, s('kaomoji_een3', t('｡ﾟ･(>﹏<)･ﾟ｡')))
table.insert(kaomoji_snippets, s('kaomoji_een4', t('(>﹏<)')))
table.insert(kaomoji_snippets, s('kaomoji_een5', t('(つ﹏<)･ﾟ｡')))
table.insert(kaomoji_snippets, s('kaomoji_oyasumi', t('( ˊ࿁ˋ ) ᐝ')))
vim.list_extend(kaomoji_snippets, sm({ 'kaomoji_oyasumi1', 'kaomoji_sleep' }, t('⊂´⌒つ* -ω-)っ')))
table.insert(kaomoji_snippets, s('kaomoji_nikkori', t('(  ◜◡◝  )')))
table.insert(kaomoji_snippets, s('kaomoji_yoshi', t('(*•̀ᴗ•́*)و ̑̑')))
vim.list_extend(kaomoji_snippets, sm({ 'kaomoji_haato', 'kaomoji_heart' }, t('(*˘︶˘*).｡.:*♡')))
vim.list_extend(kaomoji_snippets, sm({ 'kaomoji_haato1', 'kaomoji_heart1' }, t('( ⁎ᵕᴗᵕ⁎ )')))
table.insert(kaomoji_snippets, s('kaomoji_huhun', t('(๑˘︶˘)')))
table.insert(kaomoji_snippets, s('kaomoji_yoroshiku', t('(ㅅ •͈ᴗ•͈)')))

return { snippets = kaomoji_snippets, autosnippets = {} }
