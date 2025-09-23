-- Import all XAML snippet modules
local list = require('utils.list')

return {
  snippets = list.concat(
    require('luasnippets.xaml.namespace'),
    require('luasnippets.xaml.xaml'),
    require('luasnippets.xaml.tag_general'),
    require('luasnippets.xaml.template_tag'),
    require('luasnippets.xaml.template_attr'),
    require('luasnippets.xaml.tag'),
    require('luasnippets.xaml.xml_namespace')
  ),
  autosnippets = {},
}
