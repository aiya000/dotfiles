local list = require('utils.list')

return list.concat(
  require('snippets.tex.math'),
  require('snippets.tex.tex')
)