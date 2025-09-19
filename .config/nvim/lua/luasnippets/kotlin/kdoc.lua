local fmt = require('luasnip.extras.fmt').fmt
local list = require('utils.list')
local ls = require('luasnip')
local utils = require('utils.luasnip')

local sm = utils.sm
local s = ls.snippet
local i = ls.insert_node
local t = ls.text_node

return {
  sm({'kdoc_comment_block', 'document_comment_block', 'doc'}, fmt([[
    /**
     * {content}
     */
  ]], {
    content = i(1, ''),
  })),

  sm({'kdoc_param', 'param'}, fmt([[
    @param {param_name} {description}
  ]], {
    param_name = i(1, 'paramName'),
    description = i(2, 'description'),
  })),

  sm({'kdoc_property', 'property'}, t('@property')),

  s('kdoc_constructor', t('@constructor')),

  sm({'kdoc_see', 'see'}, fmt([[
    @see {identifier}
  ]], {
    identifier = i(1, 'identifier'),
  })),

  sm({'kdoc_throws', 'throws'}, fmt([[
    @throws {exception_name} {when}
  ]], {
    exception_name = i(1, 'ExceptionName'),
    when = i(2, 'when'),
  })),
}