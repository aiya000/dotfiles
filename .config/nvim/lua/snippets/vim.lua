local list = require('utils.list')

return list.concat(
  require('snippets.vim.vim-check'),
  require('snippets.vim.vital'),
  require('snippets.vim.vim'),
  require('snippets.vim.plugins')
)