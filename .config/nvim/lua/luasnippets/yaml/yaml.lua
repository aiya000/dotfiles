local ls = require('luasnip')
local s = ls.snippet
local t = ls.text_node
local i = ls.insert_node
local fmt = require('luasnip.extras.fmt').fmt
local sm = ls.extend_decorator.apply(s, {}, { common = { }, condition = function() return true end })

return {
  -- stack template config
  s("template-stack-config", fmt([[
templates:
  params:
    author-email: {}
    author-name: {}
    copyright: 2015, {}
    github-username: {}
    category: {}]], {
    i(1, "aiya000.develop@gmail.com"),
    i(2, "aiya000"),
    i(3),
    i(4),
    i(0, "Web")
  })),
}