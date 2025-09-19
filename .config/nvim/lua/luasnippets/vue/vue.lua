local ls = require('luasnip')
local s = ls.snippet
local t = ls.text_node
local i = ls.insert_node
local fmt = require('luasnip.extras.fmt').fmt
local sm = ls.extend_decorator.apply(s, {}, { common = { }, condition = function() return true end })

return {
  -- vue class attribute
  sm({
    {trig = "typescript_class", dscr = "typescript class attribute"},
    {trig = "cl", dscr = "typescript class attribute"},
  }, fmt("class=\"{}\"", {
    i(0, "name")
  })),

  -- scss class
  s("scss_class", fmt(".{} {{{}}}", {
    i(1, "name"),
    i(0, "#:here")
  })),

  -- template
  s("template", fmt("<template>{}</template>", {
    i(0)
  })),

  -- keepalive
  s("keep_alive", fmt("<KeepAlive>{}</KeepAlive>", {
    i(0)
  })),

  -- teleport
  s("teleport", fmt("<Teleport to=\"{}\">{}</Teleport>", {
    i(1, "body"),
    i(0)
  })),

  -- i18n
  s("i18n_lang_yaml", fmt("<i18n lang=\"yaml\">{}</i18n>", {
    i(0, "#:here")
  })),

  -- script setup
  s("script_setup_lang_ts", fmt("<script setup lang=\"ts\">{}</script>", {
    i(0, "here")
  })),

  -- style scoped
  s("style_lang_scss_scoped", fmt("<style lang=\"scss\" scoped>{}</style>", {
    i(0, "here")
  })),

  -- suspense template
  s("suspense_template", fmt([[
<Suspense>
  <template #default>
    {}
  </template>
  <template #fallback>
    {}
  </template>
</Suspense>]], {
    i(1, "#:here"),
    i(0, "#:here")
  })),

  -- computed
  s("computed", fmt("computed(() => {})", {
    i(0, "#:here")
  })),

  -- define props
  s("define_props", fmt("const {{ {} }} = defineProps<{{{}}}>(){}", {
    i(2, "props"),
    i(1, "here"),
    i(0)
  })),

  -- define emits
  s("define_emits", fmt("const emit = defineEmits<{{{}}}>(){}", {
    i(1, "here"),
    i(0)
  })),

  -- watch
  s("watch", fmt("watch({}, () => {})", {
    i(1, "sources"),
    i(0)
  })),

  -- watchEffect
  s("watchEffect", fmt("watchEffect(() => {})", {
    i(0)
  })),

  -- onMounted
  s("onMounted", fmt("onMounted(() => {})", {
    i(0)
  })),

  -- onUnmounted
  s("onUnmounted", fmt("onUnmounted(() => {})", {
    i(0)
  })),
}