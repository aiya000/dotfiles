local list = require('utils.list')
local ls = require('luasnip')
local s = ls.snippet
local t = ls.text_node
local i = ls.insert_node

-- Helper function to create snippets with multiple triggers
local function sm(triggers, nodes)
  local snippets = {}
  for _, trigger in ipairs(triggers) do
    table.insert(snippets, s(trigger, nodes))
  end
  return snippets
end

local subjects_snippets = list.concat({
  sm({"subject_with_diamond", "subject_title"}, {
    t("# :diamond_shape_with_a_dot_inside: "), i(1, "project-name"), t(" :diamond_shape_with_a_dot_inside:")
  }),

  sm({"subject_with_dizzy", "subject_usage"}, {
    t("# :dizzy: "), i(1, "Usage"), t(" :dizzy:")
  }),

  sm({"subject_with_gift", "subject_how_to_install_this"}, {
    t("# :gift: "), i(1, "How to install this"), t(" :gift:")
  }),

  sm({"subject_with_pray", "subject_thanks"}, {
    t("# :pray: "), i(1, "Thanks"), t(" :pray:")
  }),

  s("subject_with_relaxed", {
    t("# :relaxed: "), i(1, "subject description"), t(" :relaxed:")
  }),

  sm({"subject_with_gray_exclamation", "subject_requirements"}, {
    t("# :grey_exclamation: "), i(1, "Requirements"), t(" :grey_exclamation:")
  }),

  sm({"subject_with_muscle", "subject_example"}, {
    t("# :muscle: "), i(1, "Example"), t(" :muscle:")
  }),

  sm({"subject_with_wrench", "subject_support"}, {
    t("# :wrench: "), i(1, "Support"), t(" :wrench:")
  }),

  sm({"subject_with_question", "subject_why"}, {
    t("# :question: "), i(1, "Why ...?"), t(" :question:")
  })
})

return subjects_snippets