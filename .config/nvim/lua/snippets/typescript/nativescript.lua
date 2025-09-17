local fmt = require('luasnip.extras.fmt').fmt
local list = require('utils.list')
local ls = require('luasnip')
local utils = require('utils.luasnip')

local sm = utils.sm
local s = ls.snippet
local i = ls.insert_node

return sm({'nantivescript_basic_component', 'component_basic', 'basic_component'}, fmt([[
  import {{ Component, Vue }} from 'vue-property-decorator'

  @Component
  export default class {Name} extends Vue {{}}
]], {
  Name = i(1, 'Name'),
}))