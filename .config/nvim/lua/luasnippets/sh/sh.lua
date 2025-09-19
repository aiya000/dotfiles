local ls = require('luasnip')
local s = ls.snippet
local t = ls.text_node
local i = ls.insert_node
local fmt = require('luasnip.extras.fmt').fmt
local sm = ls.extend_decorator.apply(s, {}, { common = { }, condition = function() return true end })

return {
  -- shebangs
  sm({
    {trig = "bash_shebang", dscr = "bash shebang"},
    {trig = "shebang", dscr = "bash shebang"},
    {trig = "shebang_bash", dscr = "bash shebang"},
  }, {
    t("#!/bin/bash")
  }),

  sm({
    {trig = "bats_shebang", dscr = "bats shebang"},
    {trig = "shebang_bats", dscr = "bats shebang"},
  }, {
    t("#!/usr/bin/env bats")
  }),

  -- syntaxes
  s("if", fmt([[
if {} ; then
  {}
fi]], {
    i(1, "#:condtion"),
    i(0)
  })),

  s("else", fmt([[
else
  {}]], {
    i(0, "TARGET")
  })),

  s("elif", fmt([[
elif {} ; then
  {}]], {
    i(1, "#:condition"),
    i(0, "TARGET")
  })),

  s("for", fmt([[
for {} in {} ; do
  {}
done]], {
    i(1, "x"),
    i(2, "#:var"),
    i(0)
  })),

  s("for_index", fmt([[
for (( {}; {}; {} )) ; do
  {}
done]], {
    i(1, "#:init"),
    i(2, "#:condtion"),
    i(3, "#:effect"),
    i(0)
  })),

  s("while", fmt([[
while {} ; do
  {}
done]], {
    i(1, "#:condition"),
    i(0, "TARGET")
  })),

  s("case", fmt([[
case {} in
  {}
esac]], {
    i(1),
    i(0)
  })),

  s("case_pattern", fmt([[
{})
  {}
;;]], {
    i(1),
    i(0)
  })),

  s("case_pattern_wildcard", fmt([[
*)
  {}]], {
    i(0)
  })),

  s("template_case", fmt([[
case {} in
{})
  {}
;;
*)
  {}
esac]], {
    i(1),
    i(2, "pattern"),
    i(3),
    i(0, "echo \"$$1 Didn't match anything\"")
  })),

  -- array operations
  sm({
    {trig = "array_length", dscr = "array length"},
    {trig = "array_size", dscr = "array length"},
  }, fmt("${{#${{{}}}}}{}",{
    i(1, "array_name"),
    i(0)
  })),

  s("array_whole", fmt("${{${{{}}}[@]}}{}",{
    i(1, "array_name"),
    i(0)
  })),

  s("array_index", fmt("${{${{{}}}[{}]}}",{
    i(1, "array_name"),
    i(2, "0")
  })),

  sm({
    {trig = "array_append", dscr = "array append"},
    {trig = "array_push", dscr = "array append"},
    {trig = "array_add", dscr = "array append"},
  }, fmt("{}+=(\"{}\")",{
    i(1, "array"),
    i(0)
  })),

  s("array_join", fmt([[
{}="$(printf "%s{}" "${{{}[@]}}")"
{}=${{{}%{}}}{}]], {
    i(1, "new_array"),
    i(2, "separator"),
    i(3, "old_array"),
    i(4),
    i(5),
    i(6),
    i(0)
  })),

  -- arguments
  sm({
    {trig = "args_length", dscr = "arguments length"},
    {trig = "args_size", dscr = "arguments length"},
  }, {
    t("$#")
  }),

  s("args_whole", {
    t("$@")
  }),

  -- templates
  s("array_from_string", fmt("IFS='{}' read -ra {} <<< \"{}\"{}",{
    i(2, " "),
    i(1, "var_name"),
    i(3, "str_split_by_ifs"),
    i(0)
  })),

  s("array_from_cmd_lines", fmt([[
{}=()
while IFS='' read -r line ; do
  {}+=("$line") ;
done < <({}){}]], {
    i(1, "array"),
    i(2),
    i(3, "cmd"),
    i(0)
  })),

  sm({
    {trig = "read_lines_from_pipe", dscr = "read lines from pipe"},
    {trig = "while_lines", dscr = "read lines from pipe"},
  }, fmt([[
while read -r {} ; do
  {}
done]], {
    i(1, "line"),
    i(0)
  })),

  sm({
    {trig = "prompt", dscr = "prompt for input"},
    {trig = "prompt_for_input", dscr = "prompt for input"},
    {trig = "ask_for_input", dscr = "prompt for input"},
  }, fmt("read -rp '{}' {}",{
    i(1, "message"),
    i(0, "var")
  })),

  sm({
    {trig = "which", dscr = "command exists check"},
    {trig = "command_v", dscr = "command exists check"},
  }, fmt("command -v {} &> /dev/null",{
    i(1, "cmd")
  })),

  sm({
    {trig = "define_sourced_dir", dscr = "define script directory"},
    {trig = "define_this_script_dir", dscr = "define script directory"},
  }, fmt("{}=\"$(cd -- \"$(dirname -- \"${{BASH_SOURCE:-\\$0}}\")\" && pwd || exit 1)\"",{
    i(0, "this_script_dir")
  })),

  s("assign_array", fmt("{}=(\"{}\")",{
    i(1, "new_array"),
    i(0, "source_array")
  })),

  s("export_function", fmt("export -f {}",{
    i(0, "func_name")
  })),

  s("poi", fmt("echo \"poi: ${{{}}}\"",{
    i(0, "var_name")
  })),
}