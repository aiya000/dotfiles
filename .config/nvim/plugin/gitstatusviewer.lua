vim.api.nvim_create_user_command('GitStatusViewer', function(opts)
  vim.fn['gitstatusviewer#git_status_viewer'](opts.args)
end, {
  bar = true,
  nargs = '*',
})
