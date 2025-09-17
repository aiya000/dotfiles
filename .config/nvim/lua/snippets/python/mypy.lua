local ls = require("luasnip")
local s = ls.snippet
local t = ls.text_node

return {
  s("mypy_ignore_this_line", t("# type: ignore")),

  s("mypy_ignore_all", t("# mypy: ignore")),
}