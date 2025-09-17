local list = require('utils.list')

return list.concat(
  require('snippets.sh.shellcheck'),
  require('snippets.sh.sh'),
  require('snippets.sh.bash')
)