local ls = require('luasnip')
local s = ls.snippet
local t = ls.text_node
local i = ls.insert_node

local M = {
  -- Character dialogue snippets
  s({ 'watashi_saying', 'watashi' }, {
    t('私「'),
    i(0, 'here'),
    t('」'),
  }),

  s({ 'nico_saying', 'nico' }, {
    t('にこ「'),
    i(0, 'here'),
    t('」'),
  }),

  s({ 'maki_saying', 'maki' }, {
    t('真姫「'),
    i(0, 'here'),
    t('」'),
  }),

  s('maki_thinking', {
    t('真姫（'),
    i(0, 'here'),
    t('）'),
  }),

  s({ 'hanayo', 'pana' }, {
    t('花陽「'),
    i(0, 'here'),
    t('」'),
  }),

  s('rin', {
    t('凛「'),
    i(0, 'here'),
    t('」'),
  }),

  -- Basic templates (simplified)
  s('mu', {
    t('//talkright[mu]{'),
    t('\n'),
    i(0),
    t('\n'),
    t('//}'),
  }),

  s('focus', {
    t('//focus'),
  }),

  s('attention', {
    t('//attention{'),
    t('\n'),
    i(0),
    t('\n'),
    t('//}'),
  }),

  s({ 'attention_silence', 'silence' }, {
    t('//attention{'),
    t('\n'),
    t('……'),
    t('\n'),
    t('//}'),
  }),

  s('mathcode', {
    t('@<mathcode>{'),
    i(0, 'here'),
    t('}'),
  }),

  s({ 'mathcode_sub_space', 'sub_space' }, {
    t('_!'),
  }),

  s({ 'censored1', 'ruby_censored1' }, {
    t('@<ruby>{■, CENSORED}'),
  }),
}

return M
