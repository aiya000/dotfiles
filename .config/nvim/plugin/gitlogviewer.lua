local gitlogviewer = require('gitlogviewer')

vim.api.nvim_create_user_command('GitLogViewer', function(opts)
  gitlogviewer.git_log_viewer(opts.args)
end, {
  bar = true,
  nargs = '*',
})
