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

local kaomoji_snippets = list.concat({
  s("kaomoji_ehehe", t("(,,>᎑<,,)")),
  s("kaomoji_ehehe1", t("(,,･ ･,,)")),
  s("kaomoji_ehehe2", t("(๑>◡<๑)")),
  s("kaomoji_ehehe3", t("(>᎑<`๑)")),
  s("kaomoji_ehehe4", t("(ˊo̴̶̷̤ ᴗ o̴̶̷̤ˋ)")),
  s("kaomoji_ehehe5", t("(ू•‧̫•ू⑅)")),
  s("kaomoji_wai", t("⸜(* ॑꒳ ॑* )⸝")),
  sm({"kaomoji_wai1", "kaomoji_chomado_san"}, t("(((o(*ﾟ▽ﾟ*)o)))")),
  s("kaomoji_no_emote", t("(*'-')")),
  s("kaomoji_hi", t("(*'-')ﾉ~")),
  s("kaomoji_hi1", t("(✿╹◡╹)ﾉ")),
  s("kaomoji_hi2", t("ㄟ( ･ө･ )ㄏ")),
  s("kaomoji_hamster", t("(๑╹ᆺ╹)")),
  s("kaomoji_zombi", t("ԅ(¯﹃¯ԅ))))")),
  s("kaomoji_een", t("(৹˃ᗝ˂৹)")),
  s("kaomoji_een1", t("(;>_<;)")),
  s("kaomoji_een2", t("(ó﹏ò｡)")),
  s("kaomoji_een3", t("｡ﾟ･(>﹏<)･ﾟ｡")),
  s("kaomoji_een4", t("(>﹏<)")),
  s("kaomoji_een5", t("(つ﹏<)･ﾟ｡")),
  s("kaomoji_oyasumi", t("( ˊ࿁ˋ ) ᐝ")),
  sm({"kaomoji_oyasumi1", "kaomoji_sleep"}, t("⊂´⌒つ* -ω-)っ")),
  s("kaomoji_nikkori", t("(  ◜◡◝  )")),
  s("kaomoji_yoshi", t("(*•̀ᴗ•́*)و ̑̑")),
  sm({"kaomoji_haato", "kaomoji_heart"}, t("(*˘︶˘*).｡.:*♡")),
  sm({"kaomoji_haato1", "kaomoji_heart1"}, t("( ⁎ᵕᴗᵕ⁎ )")),
  s("kaomoji_huhun", t("(๑˘︶˘)")),
  s("kaomoji_yoroshiku", t("(ㅅ •͈ᴗ•͈)"))
})

return kaomoji_snippets