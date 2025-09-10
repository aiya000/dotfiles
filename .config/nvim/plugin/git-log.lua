local gitlog = require('git-log')

vim.api.nvim_create_user_command('GitLog', function(opts)
  gitlog.git_log(opts.args)
end, {
  bar = true,
  nargs = '*',
})
