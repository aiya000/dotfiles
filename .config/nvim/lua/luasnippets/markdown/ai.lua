local fmt = require('luasnip.extras.fmt').fmt
local list = require('utils.list')
local ls = require('luasnip')
local sm = require('utils.luasnip').sm

local f = ls.function_node
local i = ls.insert_node
local s = ls.snippet
local t = ls.text_node

---現在のプロジェクト名を推論する
---@return string --gitリポジトリならメインのworktreeのルートのディレクトリ名、そうでなければカレントディレクトリのディレクトリ名
local function detect_project_name()
  local git_root = require('git').read_main_git_root()
  local project_dir = git_root ~= nil and git_root or vim.fn.getcwd()
  return vim.fn.fnamemodify(project_dir, ':t')
end

return list.concat(
  -- AI Agents General
  {
    s(
      'instruct_for_git_commit_ai_agents_general',
      t('`~/.dotfiles/.claude_global/commands/git-commit.md`のルールを参照して、`git commit`を実行して')
    ),
  },

  -- Claude Code
  sm(
    { 'instruct_for_autocompact', 'autocompact' },
    t('/compact 昔の履歴は圧縮して、最近の履歴は圧縮しないようにして')
  ),
  {
    s(
      'instruct_rename_this_project_now_claude_code',
      f(function()
        return ('/rename %s (since %s)'):format(detect_project_name(), os.date('%Y-%m-%d %H:%M'))
      end)
    ),
  },

  -- Kiro CLI
  {
    s(
      'instruct_resume_kiro_cli',
      t('/chat') -- Kiro CLIで過去セッション一覧を表示して、選択し、resumeするには、これ。いや、わからんくない？
    ),
  }
)
