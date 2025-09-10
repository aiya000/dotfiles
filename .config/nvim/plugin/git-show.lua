local gitshow = require('git-show')

vim.api.nvim_create_user_command('GitShow', function(opts)
  gitshow.git_show(opts.args)
end, {
  bar = true,
  nargs = '*',
})
