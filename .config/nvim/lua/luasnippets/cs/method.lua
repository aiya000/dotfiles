local list = require('utils.list')
local ls = require('luasnip')
local s = ls.snippet
local t = ls.text_node
local i = ls.insert_node
local fmt = require('luasnip.extras.fmt').fmt

-- Helper function to create snippets with multiple triggers
local function sm(triggers, nodes)
  local snippets = {}
  for _, trigger in ipairs(triggers) do
    table.insert(snippets, s(trigger, nodes))
  end
  return snippets
end

local method_snippets = list.concat({
  sm({"messagebox", "msgbox"}, {
    t("MessageBox.Show("), i(1, "#:text"), t(", "), i(2, "#:caption"), t(", MessageBoxButtons."), i(3, "OK"), t(", MessageBoxIcon."), i(4, "Information"), t(");"), i(5, "")
  }),

  sm({"messagebox_yesno", "msgbox_yesno"}, {
    t("MessageBox.Show("), i(1, "#:text"), t(", "), i(2, "#:caption"), t(", MessageBoxButtons.YesNo, MessageBoxIcon.Question);"), i(3, "")
  }),

  s("inputbox", {
    t("Interaction.InputBox("), i(1, "#:Prompt"), t(", "), i(2, "#:Title"), t(', "'), i(3, "#:DefaultResponse"), t('");'), i(4, "")
  }),

  s("DebugPrint", {
    t("System.Diagnostics.Debug.WriteLine("), i(1, ""), t(");")
  }),

  s("Stopwatch.StartNew", t("System.Diagnostics.Stopwatch.StartNew()")),

  -- WPF
  sm({"messagebox_wpf", "msgbox_wpf"}, {
    t("MessageBox.Show("), i(1, "#:text"), t(", "), i(2, "#:caption"), t(", MessageBoxButton."), i(3, "OK"), t(", MessageBoxImage."), i(4, "Information"), t(");"), i(5, "")
  }),

  sm({"messagebox_yesno_wpf", "msgbox_yesno_wpf"}, {
    t("MessageBox.Show("), i(1, "#:text"), t(", "), i(2, "#:caption"), t(", MessageBoxButton.YesNo, MessageBoxImage.Question);"), i(3, "")
  })
})

return method_snippets