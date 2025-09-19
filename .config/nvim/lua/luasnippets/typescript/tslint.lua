local fmt = require('luasnip.extras.fmt').fmt
local list = require('utils.list')
local ls = require('luasnip')
local utils = require('utils.luasnip')

local sm = utils.sm
local s = ls.snippet
local i = ls.insert_node

return list.concat(
  {
    s('tslint_disable', fmt([[
      /* tslint:disable */
    ]], {})),

    s('tslint_enable', fmt([[
      /* tslint:enable */
    ]], {})),

    s('tslint_disable_rules', fmt([[
      /* tslint:disable:{rules_separated_by_space} */
    ]], {
      rules_separated_by_space = i(1, 'rules_separated_by_space'),
    })),

    s('tslint_enable_rules', fmt([[
      /* tslint:enable:{rules_separated_by_space} */
    ]], {
      rules_separated_by_space = i(1, 'rules_separated_by_space'),
    })),

    s('tslint_disable_next_lint', fmt([[
      // tslint:disable-next-line
    ]], {})),

    s('tslint_disable_next_lint_rules', fmt([[
      // tslint:disable-next-line:{rules_separated_by_space}
    ]], {
      rules_separated_by_space = i(1, 'rules_separated_by_space'),
    })),
  },

  sm({'tslint_disable_line', 'tslint_disable_left'}, fmt([[
    // tslint:disable-line
  ]], {})),

  sm({'tslint_disable_line_rules', 'tslint_disable_left_rules', 'tslint_suppress', 'suppress_tslint'}, fmt([[
    // tslint:disable-line:{rules_separated_by_space}
  ]], {
    rules_separated_by_space = i(1, 'rules_separated_by_space'),
  })),

  sm({'tslint_ignore', 'ignore_tslint'}, fmt([[
    /* tslint:disable:{rules_separated_by_space} */
    {}
    /* tslint:enable:{rules_separated_by_space} */
  ]], {
    rules_separated_by_space = i(1, 'rules_separated_by_space'),
    i(2, ''),
  }))
)