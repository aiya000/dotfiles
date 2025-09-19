local ls = require('luasnip')
local s = ls.snippet
local t = ls.text_node

local import_snippets = {
  s("s.r.c", t("System.Runtime.CompilerServices")),

  s("m.v", t("Microsoft.VisualBasic")),

  s("UnitTesting", t("Microsoft.VisualStudio.TestTools.UnitTesting;")),

  s("Friendly", t("Codeer.Friendly;")),

  s("FriendlyWindows;", t("Codeer.Friendly.Windows;")),

  s("FriendlyDynamic;", t("Codeer.Friendly.Dynamic;")),

  s("FriendlyWindowsGrasp;", t("Codeer.Friendly.Windows.Grasp;")),

  s("FriendlyWPFStandardControls;", t("RM.Friendly.WPFStandardControls;"))
}

return import_snippets