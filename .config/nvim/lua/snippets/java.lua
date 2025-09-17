local list = require('utils.list')

return list.concat(
  require('snippets.java.android'),
  require('snippets.java.java'),
  require('snippets.java.javadoc'),
  require('snippets.java.javafx')
)