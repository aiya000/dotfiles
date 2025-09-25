local fmt = require('luasnip.extras.fmt').fmt
local list = require('utils.list')
local ls = require('luasnip')
local utils = require('utils.luasnip')

local sm = utils.sm
local s = utils.s
local i = ls.insert_node
local t = ls.text_node

return list.concat(
  -- shellcheck disable with placeholder
  sm(
    { 'shellcheck_disable', 'disable_shellcheck' },
    fmt('# shellcheck disable={}', {
      i(1, 'SC1090'),
    })
  ),

  -- specific shellcheck disable comments
  {
    s('shellcheck_disable_cannot_follow_non_constant_source',
      t('# shellcheck disable=SC1090')
    ),

    s('shellcheck_disable_cd_failing_cases',
      t('# shellcheck disable=SC2164')
    ),

    s('shellcheck_disable_check_exit_code_directory',
      t('# shellcheck disable=SC2181')
    ),

    s('shellcheck_disable_check_tilde_does_not_expand_in_quotes',
      t('# shellcheck disable=SC2088')
    ),

    s('shellcheck_disable_check_prefer_mapfile_or_read__a',
      t('# shellcheck disable=SC2207')
    ),
  }
)
