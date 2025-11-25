local list = require('utils.list')
local ls = require('luasnip')
local fmt = require('luasnip.extras.fmt').fmt
local sm = require('utils.luasnip').sm
local s = ls.snippet
local i = ls.insert_node
local t = ls.text_node
local f = ls.function_node

return list.concat(
  {
    s(
      'date',
      f(function()
        return os.date('%Y-%m-%d')
      end)
    ),
  },

  {
    s('to', t('=>')),
    s('readme', t('README')),
    s('ari', t('ありがとうございます')),
    s('ariga', t('ありがとう')),
    s('yoro', t('よろしくお願いします')),
    s('mousi', t('申し訳ございません')),
    s('otu', t('お疲れ様です')),
  },

  -- Re:VIEW
  {
    s(
      'range_surround',
      fmt([[
        -- #@@range_begin({range_name})
        -- #@@range_end({range_name})
      ]], {
        range_name = i(1, 'range_name'),
      })
    ),

    s(
      'range_begin',
      fmt('-- #@@range_begin({})', {
        i(1, 'range_name'),
      })
    ),

    s(
      'range_end',
      fmt('-- #@@range_end({})', {
        i(1, 'range_name'),
      })
    ),
  }
)
