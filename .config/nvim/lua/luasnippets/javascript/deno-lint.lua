local fmt = require('luasnip.extras.fmt').fmt
local list = require('utils.list')
local ls = require('luasnip')
local utils = require('utils.luasnip')

local sm = utils.sm
local s = ls.snippet
local i = ls.insert_node

return list.concat(
  sm(
    { 'deno_lint_ignore', 'deno_lint_ignore_next_line' },
    fmt(
      [[
    // deno-lint-ignore {}
  ]],
      {
        i(1, 'rules-separated-by-space'),
      }
    )
  ),

  sm(
    { 'deno_lint_ignore_file', 'deno_lint_ignore_all', 'deno_lint_disable_all' },
    fmt(
      [[
    // deno-lint-ignore-file {}
  ]],
      {
        i(1, 'rules-separated-by-space'),
      }
    )
  )
)
