local fmt = require('luasnip.extras.fmt').fmt
local list = require('utils.list')
local ls = require('luasnip')
local utils = require('utils.luasnip')

local sm = utils.sm
local s = ls.snippet
local i = ls.insert_node
local t = ls.text_node

local android_snippets = {}

-- Android layout templates
table.insert(android_snippets, s('LinearLayout_surround_template', fmt([[
  <LinearLayout xmlns:android="http://schemas.android.com/apk/res/android"
      android:layout_width="match_parent"
      android:layout_height="match_parent"
      android:orientation="vertical">
      {}
  </LinearLayout>
]], {
  i(1, ''),
})))

return { snippets = android_snippets, autosnippets = {} }