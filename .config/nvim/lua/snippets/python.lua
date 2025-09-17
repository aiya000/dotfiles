local list = require('utils.list')

return list.concat(
  require('snippets.python.python'),
  require('snippets.python.docstring'),
  require('snippets.python.mypy')
)