local list = require('utils.list')

return list.concat(
  require('snippets.typescript.gas'),
  require('snippets.typescript.typo'),
  require('snippets.typescript.zod'),
  require('snippets.typescript.log4js'),
  require('snippets.typescript.playwright'),
  require('snippets.typescript.tslint'),
  require('snippets.typescript.typedoc'),
  require('snippets.typescript.vue'),
  require('snippets.typescript.jest'),
  require('snippets.javascript.eslint'),
  require('snippets.javascript.deno-lint'),
  require('snippets.typescript.typescript')
)
