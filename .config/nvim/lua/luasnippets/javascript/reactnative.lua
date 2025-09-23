local fmt = require('luasnip.extras.fmt').fmt
local list = require('utils.list')
local ls = require('luasnip')
local utils = require('utils.luasnip')

local sm = utils.sm
local s = ls.snippet
local i = ls.insert_node
local t = ls.text_node

-- Meta

return list.concat(
  sm(
    { 'A_tag', 'at' },
    fmt('<{}/>', {
      i(1, 'ComponentName'),
    })
  ),

  sm(
    { 'A_tag_surround', 'ats' },
    fmt('<{component}>{}</{component}>', {
      component = i(1, 'ComponentName'),
      i(2, ''),
    })
  ),

  sm(
    { 'A_attribute', 'aa' },
    fmt('{attr_name}={value}', {
      attr_name = i(1, 'attr_name'),
      value = i(2, 'value'),
    })
  ),

  -- JSXes
  {
    s(
      'import_react_bases',
      fmt(
        [[
      import React, {{ Component }} from 'react'
      import {{ AppRegistry }} from 'react-native'
    ]],
        {}
      )
    ),

    s('export_default', t('export default')),

    s(
      'comment_jsx',
      fmt('{{/* {} */}}', {
        i(1, ''),
      })
    ),

    s(
      'Component_template',
      fmt(
        [[
      class {name} extends Component {{
        render() {{
          return (
              {}
          )
        }}
      }}
    ]],
        {
          name = i(1, 'Name'),
          i(2, '#:JSX'),
        }
      )
    ),

    s(
      'render_template',
      fmt(
        [[
      render() {{
        return (
            {}
        )
      }}
    ]],
        {
          i(1, ''),
        }
      )
    ),

    s(
      'AppRegistry_template',
      fmt("AppRegistry.registerComponent('{project}', () => {component})", {
        project = i(1, 'ProjectName'),
        component = i(2, 'ComponentName'),
      })
    ),

    s(
      'Stylesheet_template',
      fmt(
        [[
      const styles = StyleSheet.create({{
          {name}: {{
              {}
          }},
      }})
    ]],
        {
          name = i(1, 'name'),
          i(2, ''),
        }
      )
    ),
  },

  -- Callings
  {
    s(
      'ToastAndroid_show',
      fmt("ToastAndroid.show('{message}', ToastAndroid.{duration})", {
        message = i(1, 'message'),
        duration = i(2, 'LONG'),
      })
    ),
  },

  -- Tags
  {
    s(
      'Text',
      fmt('<Text>{}</Text>', {
        i(1, 'hello'),
      })
    ),

    s(
      'Image_template',
      fmt('<Image source={{{}}}/>', {
        i(1, 'attribute_object'),
      })
    ),

    s('View', t('<View/>')),

    s(
      'View_surround',
      fmt('<View>{}</View>', {
        i(1, ''),
      })
    ),

    s(
      'Provider_surround_template',
      fmt(
        [[
      <Provider store={{{store}}}>
          {}
      </Provider>
    ]],
        {
          store = i(1, ''),
          i(2, ''),
        }
      )
    ),
  },

  -- Attributes
  {
    s(
      'style',
      fmt('style={{{style}}}', {
        style = i(1, ''),
      })
    ),
  },

  sm(
    { 'style_with_inner_block', 'style_bl' },
    fmt('style={{{{{style}}}}}', {
      style = i(1, ''),
    })
  ),

  {
    s(
      'placeholder',
      fmt("placeholder='{}'", {
        i(1, ''),
      })
    ),

    s(
      'onChangeText',
      fmt('onChangeText={func}', {
        func = i(1, 'function'),
      })
    ),

    s(
      'onChangeText_bl',
      fmt('onChangeText={{{expr}}}', {
        expr = i(1, 'expression'),
      })
    ),

    s(
      'onPress',
      fmt('onPress={func}', {
        func = i(1, 'function'),
      })
    ),

    s(
      'onPress_bl',
      fmt('onPress={{{expr}}}', {
        expr = i(1, 'expression'),
      })
    ),
  },

  -- Styles
  {
    s(
      'flex',
      fmt('flex: {value}', {
        value = i(1, '1'),
      })
    ),

    s(
      'flexDirection',
      fmt("flexDirection: '{direction}'", {
        direction = i(1, 'row'),
      })
    ),

    s(
      'justifyContent',
      fmt("justifyContent: '{justify}'", {
        justify = i(1, 'center'),
      })
    ),
  }
)
