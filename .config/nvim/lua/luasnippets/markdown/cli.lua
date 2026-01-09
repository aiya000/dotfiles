local fmt = require('luasnip.extras.fmt').fmt
local list = require('utils.list')
local ls = require('luasnip')
local rep = require('luasnip.extras').rep
local sm = require('utils.luasnip').sm

local i = ls.insert_node
local s = ls.snippet
local t = ls.text_node

return list.concat(
  {
    s(
      'notify_cascade_schedule_today',
      fmt("notify-cascade {time_begin} '{title}' '{time_begin_} - {time_end}' 3h 1h 30m 15m 5m 1m", {
        time_begin = i(1, 'time_begin'),
        time_begin_ = rep(1),
        time_end = i(2, 'time_end'),
        title = i(3, 'title'),
      })
    ),

    s(
      'notify_cascade_schedule_someday',
      fmt("notify-cascade '{date} {time_begin}' '{title}' '{time_begin_} - {time_end}' 3h 1h 30m 15m 5m 1m", {
        date = i(1, 'date'),
        time_begin = i(2, 'time_begin'),
        time_begin_ = rep(2),
        time_end = i(3, 'time_end'),
        title = i(4, 'title'),
      })
    ),
  }
)
