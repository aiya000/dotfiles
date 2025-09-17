local list = require('utils.list')

return list.concat(
  require('snippets.yaml.yaml'),
  require('snippets.yaml.haskell')
)