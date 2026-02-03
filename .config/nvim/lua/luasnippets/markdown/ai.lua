local fmt = require('luasnip.extras.fmt').fmt
local list = require('utils.list')
local ls = require('luasnip')
local sm = require('utils.luasnip').sm

local i = ls.insert_node
local s = ls.snippet
local t = ls.text_node

return list.concat(
  -- TODO: なんかできない。多分smの実装？ 修正する
  -- sm({ 'ultrathink', 'ult' }, t('ultrathink'))
  {
    s('ultrathink', t('ultrathink')),
    s('ult', t('ultrathink')),
  },

  {
    s(
      'instruct_for_autocompact',
      t('/compact 昔の履歴は圧縮して、最近の履歴は圧縮しないようにして')
    ),
    s('autocompact', t('/compact 昔の履歴は圧縮して、最近の履歴は圧縮しないようにして')),
  },

  {
    s(
      'instruct_for_git_commit',
      t(
        '`~/.dotfiles/.claude_global/commands/git-commit.md`のルールを参照して、`git commit`を実行して'
      )
    ),
    s(
      'git_commit',
      t(
        '`~/.dotfiles/.claude_global/commands/git-commit.md`のルールを参照して、`git commit`を実行して'
      )
    ),
  }
)
