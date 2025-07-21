vim.api.nvim_create_user_command('GitDiffViewer', function(opts)
  vim.fn['gitdiffviewer#git_diff_viewer'](opts.args)
end, {
  bar = true,
  nargs = '*',
})