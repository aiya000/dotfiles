local ls = require('luasnip')
local s = ls.snippet
local t = ls.text_node
local i = ls.insert_node
local fmt = require('luasnip.extras.fmt').fmt
local sm = require('utils.luasnip').sm

local rio_snippets = {}

-- RIO Has pattern
vim.list_extend(
  rio_snippets,
  sm({ 'rio_has_pattern', 'has_pattern' }, {
    t('class Has'),
    i(1, 'EnvName'),
    t(' '),
    i(2, 'envName'),
    t(' where'),
    t({ '', '    ' }),
    i(2),
    t("L :: Lens' "),
    i(2),
    t(' '),
    i(1),
    t({ '', '', 'instance Has' }),
    i(1),
    t(' '),
    i(1),
    t(' where'),
    t({ '', '    ' }),
    i(2),
    t('L = id'),
  }, { key = 'rio_has_pattern' })
)

return rio_snippets
