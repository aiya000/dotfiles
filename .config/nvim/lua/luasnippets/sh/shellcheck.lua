local ls = require('luasnip')
local s = ls.snippet
local t = ls.text_node
local i = ls.insert_node
local fmt = require('luasnip.extras.fmt').fmt
local sm = ls.extend_decorator.apply(s, {}, {
  common = {},
  condition = function()
    return true
  end,
})

return {
  -- shellcheck disable with placeholder
  sm(
    {
      { trig = 'shellcheck_disable', dscr = 'shellcheck disable comment' },
      { trig = 'disable_shellcheck', dscr = 'shellcheck disable comment' },
    },
    fmt('# shellcheck disable={}', {
      i(1, 'SC1090'),
    })
  ),

  -- specific shellcheck disable comments
  s('shellcheck_disable_cannot_follow_non_constant_source', {
    t('# shellcheck disable=SC1090'),
  }),

  s('shellcheck_disable_cd_failing_cases', {
    t('# shellcheck disable=SC2164'),
  }),

  s('shellcheck_disable_check_exit_code_directory', {
    t('# shellcheck disable=SC2181'),
  }),

  s('shellcheck_disable_check_tilde_does_not_expand_in_quotes', {
    t('# shellcheck disable=SC2088'),
  }),

  s('shellcheck_disable_check_prefer_mapfile_or_read__a', {
    t('# shellcheck disable=SC2207'),
  }),
}
