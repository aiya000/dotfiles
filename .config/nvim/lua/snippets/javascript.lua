local list = require('utils.list')

return list.concat(
  require('snippets.javascript.eslint'),
  require('snippets.javascript.deno-lint'),
  require('snippets.javascript.javascript'),
  require('snippets.javascript.surfingkeys'),
  require('snippets.javascript.template'),
  require('snippets.javascript.ts-check'),
  require('snippets.javascript.reactnative')
)