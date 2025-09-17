local ls = require('luasnip')
local s = ls.snippet
local t = ls.text_node
local i = ls.insert_node
local fmt = require('luasnip.extras.fmt').fmt
local sm = ls.extend_decorator.apply(s, {}, { common = { }, condition = function() return true end })

return {
  -- function definition
  sm({
    {trig = "function", dscr = "bash function"},
    {trig = "func", dscr = "bash function"},
    {trig = "fun", dscr = "bash function"},
  }, fmt([[
function {} () {{
    {}
}}]], {
    i(1, "#:name"),
    i(0, "TARGET")
  })),

  -- test operations
  s("test", fmt("[[ {} ]]",{
    i(0)
  })),

  s("test_regex", fmt("[[ {} =~ {} ]]",{
    i(1),
    i(0)
  })),

  -- variable replacement operations
  s("var_replace_only_first", fmt("${{${{{}}}/${{{}}}/${{{}}}}}{}",{
    i(1, "varname"),
    i(2, "pattern_str"),
    i(3, "replaced"),
    i(0)
  })),

  s("var_replace_all", fmt("${{${{{}}}//{}/${{{}}}}}{}",{
    i(1, "varname"),
    i(2, "pattern_str"),
    i(3, "replaced"),
    i(0)
  })),

  s("var_replace_head", fmt("${{${{{}}}/#{}/${{{}}}}}{}",{
    i(1, "varname"),
    i(2, "pattern_str"),
    i(3, "replaced"),
    i(0)
  })),

  s("var_replace_tail", fmt("${{${{{}}}/%{}/${{{}}}}}{}",{
    i(1, "varname"),
    i(2, "pattern_str"),
    i(3, "replaced"),
    i(0)
  })),

  s("var_or_default", fmt("${{{}:-{}}}{}",{
    i(1, "varname"),
    i(2, "default_value"),
    i(0)
  })),

  -- array slicing
  sm({
    {trig = "array_slice", dscr = "array slice"},
    {trig = "slice", dscr = "array slice"},
  }, fmt("${{{}[@]:{}:{}}}{}",{
    i(1, "array"),
    i(2, "start"),
    i(3, "end"),
    i(0)
  })),

  s("assign_sliced_array", fmt("{}=(\"${{{}[@]:{}:{}}}\"){}",{
    i(1, "new_array"),
    i(2, "old_array"),
    i(3, "start"),
    i(4, "end"),
    i(0)
  })),
}