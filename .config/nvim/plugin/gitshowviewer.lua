vim.api.nvim_create_user_command('GitShowViewer', function(opts)
  vim.fn['gitshowviewer#git_show_viewer'](opts.args)
end, {
  bar = true,
  nargs = '*',
})
