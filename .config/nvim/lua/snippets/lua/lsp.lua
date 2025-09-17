local fmt = require('luasnip.extras.fmt').fmt
local ls = require('luasnip')

local s = ls.snippet
local i = ls.insert_node

return {
  s('lsp_diagnostic_disable', fmt([[
    ---@diagnostic disable: {}
  ]], {
    i(1, ''),
  })),

  s('lsp_diagnostic_enable', fmt([[
    ---@diagnostic enable: {}
  ]], {
    i(1, ''),
  })),

  s('lsp_diagnostic_disable_section', fmt([[
    ---@diagnostic disable: {rule}

    {}

    ---@diagnostic enable: {rule}
  ]], {
    rule = i(1, 'rule'),
    i(2, ''),
  })),

  s('lsp_diagnostic_disable_next_line', fmt([[
    ---@diagnostic disable-next-line: {}
  ]], {
    i(1, ''),
  })),

  s('lsp_diagnostic_disable_next_line_undefined_field', fmt([[
    ---@diagnostic disable-next-line: undefined-field
  ]], {})),

  s('lsp_diagnostic_disable_next_line_undefined_field_nazeka', fmt([[
    ---@diagnostic disable-next-line: undefined-field --なぜか怒られるので無視する
  ]], {}))
}