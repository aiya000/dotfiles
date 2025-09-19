local fmt = require('luasnip.extras.fmt').fmt
local list = require('utils.list')
local ls = require('luasnip')
local utils = require('utils.luasnip')

local sm = utils.sm
local s = ls.snippet
local i = ls.insert_node
local t = ls.text_node

-- Reference:
-- - Conventional Commits: https://www.conventionalcommits.org/ja/v1.0.0/

-- LuaSnip from_lua loader用の正しい形式
return {
  snippets = list.concat(
    -- Include markdown snippets
    require('luasnippets.markdown').snippets,

  {
    s('feat', fmt('feat({}): {}', {
      i(1, 'scope'),
      i(2, 'description'),
    })),

    s('fix', fmt('fix({}): {}', {
      i(1, 'scope'),
      i(2, 'description'),
    })),

    s('chore', fmt('chore({}): {}', {
      i(1, 'scope'),
      i(2, 'description'),
    })),

    s('update', fmt('update({}): {}', {
      i(1, 'scope'),
      i(2, 'description'),
    })),

    s('refactor', fmt('refactor({}): {}', {
      i(1, 'scope'),
      i(2, 'description'),
    })),

    s('change', fmt('change({}): {}', {
      i(1, 'scope'),
      i(2, 'description'),
    })),

    s('hotfix', fmt('hotfix({}): {}', {
      i(1, 'scope'),
      i(2, 'description'),
    })),

    s('disable', fmt('disable({}): {}', {
      i(1, 'scope'),
      i(2, 'description'),
    })),

    s('remove', fmt('remove({}): {}', {
      i(1, 'scope'),
      i(2, 'description'),
    })),

    s('delete', fmt('delete({}): {}', {
      i(1, 'scope'),
      i(2, 'description'),
    })),

    s('move', fmt('move({}): {}', {
      i(1, 'scope'),
      i(2, 'description'),
    })),

    s('upgrade', fmt('upgrade({}): {}', {
      i(1, 'scope'),
      i(2, 'description'),
    })),

    s('revert', fmt('revert({}): {}', {
      i(1, 'scope'),
      i(2, 'description'),
    })),

    s('improve', fmt('improve({}): {}', {
      i(1, 'scope'),
      i(2, 'description'),
    })),

    s('BREAKING_CHANGE', fmt('BREAKING CHANGE: {}', {
      i(1, 'description'),
    })),
    }
  ),
  autosnippets = {}
}