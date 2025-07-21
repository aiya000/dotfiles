-- Inspired by ujihisa's vimrc
-- And deris's code (http://deris.hatenablog.jp/entry/2013/05/10/003430)
vim.api.nvim_create_user_command('GitLogViewer', function(opts)
  vim.fn['gitlogviewer#git_log_viewer'](opts.args)
end, {
  bar = true,
  nargs = '*',
})