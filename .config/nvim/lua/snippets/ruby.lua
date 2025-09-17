local list = require('utils.list')

return list.concat({
  require('snippets.ruby.ruby'),
  require('snippets.ruby.yard'),
})