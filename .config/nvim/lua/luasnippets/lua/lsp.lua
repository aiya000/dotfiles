local fmt = require('luasnip.extras.fmt').fmt
local list = require('utils.list')
local ls = require('luasnip')
local utils = require('utils.luasnip')

local sm = utils.sm
local s = ls.snippet
local i = ls.insert_node
local t = ls.text_node

return {
  s(
    'lsp_diagnostic_disable',
    fmt('---@diagnostic disable: {}', {
      i(1, ''),
    })
  ),

  s(
    'lsp_diagnostic_enable',
    fmt('---@diagnostic enable: {}', {
      i(1, ''),
    })
  ),

  s(
    'lsp_diagnostic_disable_section',
    fmt(
      [[
    ---@diagnostic disable: {rule}

    {}

    ---@diagnostic enable: {rule}
  ]],
      {
        rule = i(1, ''),
        i(2, ''),
      }
    )
  ),

  s(
    'lsp_diagnostic_disable_next_line',
    fmt('---@diagnostic disable-next-line: {}', {
      i(1, ''),
    })
  ),

  s('lsp_diagnostic_disable_next_line_undefined_field', t('---@diagnostic disable-next-line: undefined-field')),

  s(
    'lsp_diagnostic_disable_next_line_undefined_field_nazeka',
    t('---@diagnostic disable-next-line: undefined-field --なぜか怒られるので無視する')
  ),
}
