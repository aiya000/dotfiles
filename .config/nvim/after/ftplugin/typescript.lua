local nodejs = require('nodejs')

vim.opt.commentstring = ' // %s'

-- TODO: Neovim（lua）に変えたので、大丈夫かも？ 大丈夫だったら、このコメントを削除する
-- .vimrcでも同じ内容を設定しているものの、なぜかemptyになるので、設定
-- vim.opt_local.omnifunc = 'lsp#complete'

---Checks what package manager should be used (e.g., yarn, npm, bun),
---Sets it to `vim.opt.makeprg`,
---and runs `:AsyncRun` with the given subcommand and errorformat.
---
---@param subcmd string
---@param errorformat string
local function run_script(subcmd, errorformat)
  vim.opt.errorformat = errorformat

  local current_dir = vim.fn.expand('%:p:h')
  local project_root = nodejs.read_node_root_dir(current_dir)
  if project_root == nil then
    project_root = current_dir
  end

  local manager = nodejs.check_node_project_manager(project_root)
  if manager then
    vim.opt.makeprg = manager .. ' run ' .. subcmd
    -- .. ' | sed -r ''s/\x1B\[([0-9]{1,2}(;[0-9]{1,2})?)?[m|K]//g''' " sed to remove ansi colors
  end

  -- echo ':AsyncRun -cwd=' .. project_root .. ' ' .. vim.opt.makeprg:get()
  vim.cmd('AsyncRun -cwd=' .. project_root .. ' ' .. vim.opt.makeprg:get())
  vim.cmd('vertical copen 120')
end

local function run_typecheck()
  run_script('typecheck', '%f(%l,%c): %m') -- tsc's errorformat
end

---eslintは逐次出力をせずに、最後に一気に出力するので、焦らずに待とう
local function run_lint()
  run_script('lint', 'TODO: efm-langserverが使える？')
  vim.cmd('copen')
end

---@param subcmd string
local function run_dev(subcmd)
  local actual_subcmd = (subcmd == '' or subcmd == nil) and 'dev:local' or subcmd
  -- TODO: eslintのエラーフォーマットもサポートできる？
  run_script(actual_subcmd, '%f(%l,%c): %m') -- tsc's errorformat
end

local function run_all()
  run_typecheck()
  run_lint()
end

-- Create user commands
vim.api.nvim_create_user_command('RunTypeCheck', run_typecheck, {})
vim.api.nvim_create_user_command('RunLint', run_lint, {})
vim.api.nvim_create_user_command('RunAll', run_all, {})
-- TODO: Ansi Colorが削除できない
-- command! -bar -nargs=? Make call s:run_dev(<q-args>)
vim.api.nvim_create_user_command('Make', run_all, {})
