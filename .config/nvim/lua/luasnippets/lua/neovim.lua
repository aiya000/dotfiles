local fmt = require('luasnip.extras.fmt').fmt
local sm = require('utils.luasnip').sm

return
  sm({ 'home', 'stdpath_config' }, fmt([[
    vim.fn.stdpath('config')
  ]], {}))
