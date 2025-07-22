vim.opt_local.list = false
vim.opt_local.number = false
vim.opt_local.relativenumber = false

vim.keymap.set('n', 'i', "denite#do_map('open_filter_buffer')", { buffer = true, silent = true })
vim.keymap.set('n', 'a', "denite#do_map('open_filter_buffer')", { buffer = true, silent = true })
vim.keymap.set('n', 'Q', "denite#do_map('quit')", { buffer = true, silent = true })
vim.keymap.set('n', '<buffer><silent><expr>', "<CR> denite#do_map('do_action')", { buffer = true, silent = true })
vim.keymap.set('n', '<buffer><silent><expr>', "<C-[> denite#do_map('quit')", { buffer = true, silent = true })
vim.keymap.set('n', '<buffer><silent><expr>', "<C-l> denite#do_map('quit')", { buffer = true, silent = true })
vim.keymap.set('n', '<buffer><silent><expr>', "<C-j> denite#do_map('do_action')", { buffer = true, silent = true })
vim.keymap.set(
  'n',
  '<buffer><silent><expr>',
  "<C-i> denite#do_map('toggle_select_down')",
  { buffer = true, silent = true }
)

vim.call("denite#custom#source('buffer', 'matchers', ['matcher_substring'])")
vim.call("denite#custom#source('file', 'matchers', ['matcher_substring'])")
vim.call("denite#custom#source('file_rec', 'matchers', ['matcher_substring'])")
vim.call("denite#custom#source('line', 'matchers', ['matcher_substring'])")
vim.call("denite#custom#source('tag', 'matchers', ['matcher_substring'])")
vim.call("denite#custom#source('file_mru', 'matchers', ['matcher_substring', 'matcher_ignore_globs'])")
vim.call("denite#custom#source('tag', 'converters', ['converter/abbr_word'])")
