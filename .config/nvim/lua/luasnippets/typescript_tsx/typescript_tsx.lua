local fmt = require('luasnip.extras.fmt').fmt
local list = require('utils.list')
local ls = require('luasnip')
local utils = require('utils.luasnip')

local sm = utils.sm
local s = ls.snippet
local i = ls.insert_node
local t = ls.text_node

return {
  -- Components & Tags
  sm({ 'fragment', 'frag' }, fmt('<>{}</>', {
    i(1, 'here'),
  })),

  s('link', fmt('<Link href="/{}">{}</Link>', {
    i(1, 'path'),
    i(2, 'here'),
  })),

  s('link_a', fmt('<Link href="/{}"><a>{}</a></Link>', {
    i(1, 'path'),
    i(2, 'here'),
  })),

  -- Attributes
  sm({ 'class_name', 'cn' }, fmt('className="{}"', {
    i(1, 'here'),
  })),

  -- Components
  s('Component_template', fmt([[
    class {} extends Component<{}, {}> {{
      render() {{
        return (
            {}
        );
      }}
    }}
  ]], {
    i(1, 'Name'),
    i(2, 'Props'),
    i(3, '{}'),
    i(4, 'JSX'),
  })),

  s('import_react', t('import React from \'react\'')),

  -- Comments (JSX style)
  sm({ 'comment', 'com' }, fmt('{{/* {} */}}', {
    i(1, 'here'),
  })),

  s('Props_with_className', fmt([[
    interface Props {{
      className?: string{}
    }}
  ]], {
    i(1, ''),
  })),

  -- React Hooks
  s('use_effect', fmt('useEffect(() => {{{}}}, [{}])', {
    i(1, 'here'),
    i(2, 'usedResources'),
  })),

  sm({ 'use_effect_on_mounted', 'mounted' }, fmt('useEffect(() => {{{}}}, [])', {
    i(1, 'here'),
  })),

  sm({ 'use_effect_on_unmounted', 'unmounted' }, fmt('useEffect(() => {}, [])', {
    i(1, 'here'),
  })),
}