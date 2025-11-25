local fmt = require('luasnip.extras.fmt').fmt
local list = require('utils.list')
local ls = require('luasnip')
local utils = require('utils.luasnip')

local sm = utils.sm
local s = ls.snippet
local i = ls.insert_node
local t = ls.text_node
local f = ls.function_node

return list.concat(
  -- Ehehe (笑顔系)
  {
    s('kaomoji_ehehe', t('(,,>᎑<,,)')),
    s('kaomoji_ehehe1', t('(,,･ ･,,)')),
    s('kaomoji_ehehe2', t('(๑>◡<๑)')),
    s('kaomoji_ehehe3', t('(>᎑<`๑)')),
    s('kaomoji_ehehe4', t('(ˊo̴̶̷̤ ᴗ o̴̶̷̤ˋ)')),
    s('kaomoji_ehehe5', t('(ू•‧̫•ू⑅)')),
    s('kaomoji_nikkori', t('(  ◜◡◝  )')),
    s('kaomoji_huhun', t('(๑˘︶˘)')),
  },

  -- Wai (喜び系)
  {
    s('kaomoji_wai', t('⸜(* ॑꒳ ॑* )⸝')),
    s('kaomoji_wai1', t('(((o(*ﾟ▽ﾟ*)o)))')),
    s('kaomoji_yoshi', t('(*•̀ᴗ•́*)و ̑̑')),
    s('kaomoji_yoroshiku', t('(ㅅ •͈ᴗ•͈)')),
  },

  -- Hi (挨拶系)
  {
    s('kaomoji_no_emote', t("(*'-')")),
    s('kaomoji_hi', t("(*'-')ﾉ~")),
    s('kaomoji_hi1', t('(✿╹◡╹)ﾉ')),
    s('kaomoji_hi2', t('ㄟ( ･ө･ )ㄏ')),
  },

  -- Een (泣き顔系)
  {
    s('kaomoji_een', t('(৹˃ᗝ˂৹)')),
    s('kaomoji_een1', t('(;>_<;)')),
    s('kaomoji_een2', t('(ó﹏ò｡)')),
    s('kaomoji_een3', t('｡ﾟ･(>﹏<)･ﾟ｡')),
    s('kaomoji_een4', t('(>﹏<)')),
    s('kaomoji_een5', t('(つ﹏<)･ﾟ｡')),
  },

  -- Heart (ハート系)
  sm({ 'kaomoji_haato', 'kaomoji_heart' }, t('(*˘︶˘*).｡.:*♡')),
  sm({ 'kaomoji_haato1', 'kaomoji_heart1' }, t('( ⁎ᵕᴗᵕ⁎ )')),

  -- Sleep & Others (睡眠・その他)
  {
    s('kaomoji_oyasumi', t('( ˊ࿁ˋ ) ᐝ')),
    s('kaomoji_hamster', t('(๑╹ᆺ╹)')),
    s('kaomoji_zombi', t('ԅ(¯﹃¯ԅ))))')),
  },
  sm({ 'kaomoji_oyasumi1', 'kaomoji_sleep' }, t('⊂´⌒つ* -ω-)っ'))
)
