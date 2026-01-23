local fmt = require('luasnip.extras.fmt').fmt
local list = require('utils.list')
local ls = require('luasnip')
local utils = require('utils.luasnip')

local sm = utils.sm
local s = ls.snippet
local i = ls.insert_node
local t = ls.text_node

return list.concat({
  s(
    'composable',
    fmt(
      [[
      export const use{Something} = () => {{
        return {{
            {}
        }}
      }}

      export type {Something}Composable = ReturnType<typeof use{Something}>
      export const {something}InjectionKey: InjectionKey<{Something}Composable>
        = Symbol('{something}')
    ]],
      {
        Something = i(1, 'Something'),
        i(2, ''),
        something = i(3, 'something'),
      }
    )
  ),

  s(
    'cl',
    fmt('class="{}"', {
      i(1, ''),
    })
  ),

  s(
    'cla',
    fmt(
      [[
        .{name} {{
          {}
        }}
      ]],
      {
        name = i(1, 'name'),
        i(2, ''),
      }
    )
  ),

  s(
    'template',
    fmt(
      [[
      <template>
      {}
      </template>
      ]],
      {
        i(1),
      }
    )
  ),

  s(
    'keep_alive',
    fmt('<KeepAlive>{}</KeepAlive>', {
      i(0),
    })
  ),

  s(
    'teleport',
    fmt('<Teleport to="{to}">{}</Teleport>', {
      to = i(1, 'to'),
      i(0),
    })
  ),

  s(
    'i18n_lang_yaml',
    fmt('<i18n lang="yaml">{}</i18n>', {
      i(0, ''),
    })
  ),

  -- script setup
  s(
    'script_setup_lang_ts',
    fmt('<script setup lang="ts">{}</script>', {
      i(0, 'here'),
    })
  ),

  -- style scoped
  s(
    'style_lang_scss_scoped',
    fmt('<style lang="scss" scoped>{}</style>', {
      i(0, 'here'),
    })
  ),

  -- suspense template
  s(
    'suspense_template',
    fmt(
      [[
  <Suspense>
    <template #default>
      {}
    </template>
    <template #fallback>
      {}
    </template>
  </Suspense>]],
      {
        i(1, '#:here'),
        i(0, '#:here'),
      }
    )
  ),

  -- computed
  s(
    'computed',
    fmt('computed(() => {})', {
      i(0, '#:here'),
    })
  ),

  -- define props
  s(
    'define_props',
    fmt('const {{ {} }} = defineProps<{{{}}}>(){}', {
      i(2, 'props'),
      i(1, 'here'),
      i(0),
    })
  ),

  -- define emits
  s(
    'define_emits',
    fmt('const emit = defineEmits<{{{}}}>(){}', {
      i(1, 'here'),
      i(0),
    })
  ),

  -- watch
  s(
    'watch',
    fmt('watch({}, () => {})', {
      i(1, 'sources'),
      i(0),
    })
  ),

  -- watchEffect
  s(
    'watchEffect',
    fmt('watchEffect(() => {})', {
      i(0),
    })
  ),

  -- onMounted
  s(
    'onMounted',
    fmt('onMounted(() => {})', {
      i(0),
    })
  ),

  -- onUnmounted
  s(
    'onUnmounted',
    fmt('onUnmounted(() => {})', {
      i(0),
    })
  ),
})
