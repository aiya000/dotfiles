local list = require('utils.list')

return list.concat(
  require('snippets.xml.xml'),
  require('snippets.xml.android'),
  require('snippets.xml.csproj')
)