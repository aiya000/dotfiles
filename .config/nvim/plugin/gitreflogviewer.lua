vim.api.nvim_create_user_command('GitReflogViewer', function(opts)
  vim.fn['gitreflogviewer#git_reflog_viewer'](opts.args)
end, {
  bar = true,
  nargs = '*',
})