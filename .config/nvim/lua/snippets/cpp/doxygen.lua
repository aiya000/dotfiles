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

-- Basics

local doxygen_snippets = list.concat({
  sm({"doxygen_block", "dbl"}, fmt([[
/*!
 * {}
 */]], {
    i(1, "")
  })),

  s("br", t("<br>")),

  sm({"doxygen_fn", "fn"}, {
    t("@fn "), i(1, "which-does-this-function")
  }),

  sm({"doxygen_def", "def"}, {
    t("@def "), i(1, "which-does-this-macro")
  }),

  sm({"doxygen_enum", "enum"}, {
    t("@enum "), i(1, "what-does-this-mean")
  }),

  sm({"doxygen_brief", "brief"}, {
    t("@brief "), i(1, "summary-what-does-this")
  }),

  sm({"doxygen_details", "details"}, {
    t("@details "), i(1, "what-does-this")
  }),

  sm({"doxygen_param", "param"}, {
    t("@param "), i(1, "paramName"), t(" "), i(2, "how-is-this-used")
  }),

  s("doxygen_return", {
    t("@return "), i(1, "which-does-this-return")
  }),

  sm({"doxygen_sa", "sa"}, {
    t("@sa "), i(1, "which-does-this-sa")
  }),

  s("doxygen_namnespace", {
    t("@namespace "), i(1, "what-are-things-is-defined-at-here")
  }),

  s("doxygen_struct", {
    t("@struct "), i(1, "what-does-this-mean")
  }),

  s("doxygen_class", {
    t("@class "), i(1, "what-does-this-mean")
  }),

  s("doxygen_typedef", {
    t("@typedef "), i(1, "what-does-this-mean")
  }),

  -- Templates
  s("doxygen_block_brief", fmt([[
/*!
 * @brief {}
 */]], {
    i(1, "what-does-this-mean")
  }))
})

return doxygen_snippets