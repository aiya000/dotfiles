-- HTML NativeScript snippets converted from neosnippet format
local ls = require("luasnip")
local s = ls.snippet
local t = ls.text_node
local i = ls.insert_node
local fmt = require("luasnip.extras.fmt").fmt

local M = {}

-- Function to create multiple aliases for a snippet
local function sm(trigger, aliases, snippet_def)
  local result = {[trigger] = snippet_def}
  for _, alias in ipairs(aliases) do
    result[alias] = snippet_def
  end
  return result
end

-- Components
local nativescript_flexbox_layout_snippets = sm("nativescript_flexbox_layout", {"ns_flexbox_layout", "flexbox_layout", "FlexboxLayout"}, s("nativescript_flexbox_layout",
  fmt([[<FlexboxLayout>{}</FlexboxLayout>]], {
    i(1, "here")
  })
))
for k, v in pairs(nativescript_flexbox_layout_snippets) do
  M[k] = v
end

local nativescript_stack_layout_snippets = sm("nativescript_stack_layout", {"ns_stack_layout", "stack_layout", "StackLayout"}, s("nativescript_stack_layout",
  fmt([[<StackLayout>{}</StackLayout>]], {
    i(1, "here")
  })
))
for k, v in pairs(nativescript_stack_layout_snippets) do
  M[k] = v
end

local nativescript_absolute_layout_snippets = sm("nativescript_absolute_layout", {"ns_absolute_layout", "absolute_layout", "AbsoluteLayout"}, s("nativescript_absolute_layout",
  fmt([[<AbsoluteLayout>{}</AbsoluteLayout>]], {
    i(1, "here")
  })
))
for k, v in pairs(nativescript_absolute_layout_snippets) do
  M[k] = v
end

local nativescript_scroll_view_snippets = sm("nativescript_scroll_view", {"ns_scroll_view", "scroll_view", "ScrollView"}, s("nativescript_scroll_view",
  fmt([[<ScrollView>{}</ScrollView>]], {
    i(1, "here")
  })
))
for k, v in pairs(nativescript_scroll_view_snippets) do
  M[k] = v
end

local nativescript_label_snippets = sm("nativescript_label", {"ns_label"}, s("nativescript_label",
  fmt([[<Label{} />]], {
    i(1, "")
  })
))
for k, v in pairs(nativescript_label_snippets) do
  M[k] = v
end

local nativescript_button_snippets = sm("nativescript_button", {"ns_button"}, s("nativescript_button",
  fmt([[<Button{} />]], {
    i(1, "")
  })
))
for k, v in pairs(nativescript_button_snippets) do
  M[k] = v
end

local nativescript_image_snippets = sm("nativescript_image", {"ns_image"}, s("nativescript_image",
  fmt([[<Image{} />]], {
    i(1, "")
  })
))
for k, v in pairs(nativescript_image_snippets) do
  M[k] = v
end

-- Attributes
M.flex_direction_column = s("flex_direction_column", t('flexDirection="column"'))

M.justifyContent_space_around = s("justifyContent_space_around", t('justifyContent="space-around"'))

local nativescript_align_items_center_snippets = sm("nativescript_align_items_center", {"ns_align_items_center"}, s("nativescript_align_items_center", t('alignItems="center"')))
for k, v in pairs(nativescript_align_items_center_snippets) do
  M[k] = v
end

M.orientation_horizontal = s("orientation_horizontal", t('orientation="horizontal"'))

M.orientation_vertical = s("orientation_vertical", t('orientation="vertical"'))

M[":class"] = s(":class",
  fmt([[class="{}"]], {
    i(1, "binding")
  })
)

-- Convert M table to array format for LuaSnip
local snippets = {}
for _, snippet in pairs(M) do
  table.insert(snippets, snippet)
end

return { snippets = snippets, autosnippets = {} }