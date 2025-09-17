local list = require('utils.list')

return list.concat(
  require('snippets.kotlin.android'),
  require('snippets.kotlin.gradle'),
  require('snippets.kotlin.kdoc'),
  require('snippets.kotlin.kotlin'),
  require('snippets.kotlin.ktlint')
)