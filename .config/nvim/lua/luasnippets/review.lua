-- Import all Re:VIEW snippet modules
local list = require('utils.list')

return {
  snippets = list.concat(
    require("luasnippets.review.review-and-css-typesettings"),
    require("luasnippets.review.review"),
    require("luasnippets.review.mine")
  ),
  autosnippets = {}
}