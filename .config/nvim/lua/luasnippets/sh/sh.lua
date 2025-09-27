local fmt = require('luasnip.extras.fmt').fmt
local list = require('utils.list')
local ls = require('luasnip')
local utils = require('utils.luasnip')

local sm = utils.sm
local s = utils.s
local i = ls.insert_node
local t = ls.text_node

return list.concat(
    -- shebangs
    sm(
      { 'bash_shebang', 'shebang', 'shebang_bash' },
      t('#!/bin/bash')
    ),

    sm(
      { 'bats_shebang', 'shebang_bats' },
      t('#!/usr/bin/env bats')
    ),

    -- syntaxes
    {
      s('if',
        fmt([[
          if {} ; then
            {}
          fi
        ]], {
          i(1, ''),
          i(2, ''),
        })
      ),

      s('else', i(0, '')),

      s('elif',
        fmt([[
          elif {} ; then
            {}
        ]], {
          i(1, ''),
          i(2, ''),
        })
      ),

      s(
        'for',
        fmt([[
          for {} in {} ; do
            {}
          done
        ]], {
          i(1, 'x'),
          i(2, '#:var'),
          i(0),
        })
      ),

      s(
        'for_index',
        fmt([[
for (( {}; {}; {} )) ; do
  {}
done]],
          {
            i(1, '#:init'),
            i(2, '#:condtion'),
            i(3, '#:effect'),
            i(0),
          }
        )
      ),

      s(
        'while',
        fmt(
          [[
while {} ; do
  {}
done]],
          {
            i(1, '#:condition'),
            i(0, 'TARGET'),
          }
        )
      ),

      s(
        'case',
        fmt(
          [[
case {} in
  {}
esac]],
          {
            i(1),
            i(0),
          }
        )
      ),

      s(
        'case_pattern',
        fmt(
          [[
{})
  {}
;;]],
          {
            i(1),
            i(0),
          }
        )
      ),

      s(
        'case_pattern_wildcard',
        fmt(
          [[
*)
  {}]],
          {
            i(0),
          }
        )
      ),

      s(
        'template_case',
        fmt(
          [[
case {} in
{})
  {}
;;
*)
  {}
esac]],
          {
            i(1),
            i(2, 'pattern'),
            i(3),
            i(0, 'echo "$$1 Didn\'t match anything"'),
          }
        )
      ),
    },

    -- array operations
    sm(
      { 'array_length', 'array_size' },
      fmt('${{#${{{}}}}}{}', {
        i(1, 'array_name'),
        i(0),
      })
    ),

    {
      s(
        'array_whole',
        fmt('${{${{{}}}[@]}}{}', {
          i(1, 'array_name'),
          i(0),
        })
      ),

      s(
        'array_index',
        fmt('${{${{{}}}[{}]}}', {
          i(1, 'array_name'),
          i(2, '0'),
        })
      ),
    },

    sm(
      { 'array_append', 'array_push', 'array_add' },
      fmt('{}+=("{}")', {
        i(1, 'array'),
        i(0),
      })
    ),

    {
      s(
        'array_join',
        fmt(
          [[
{}="$(printf "%s{}" "${{{}[@]}}")"
{}=${{{}%{}}}{}]],
          {
            i(1, 'new_array'),
            i(2, 'separator'),
            i(3, 'old_array'),
            i(4),
            i(5),
            i(6),
            i(0),
          }
        )
      ),
    },

    -- arguments
    sm(
      { 'args_length', 'args_size' },
      t('$#')
    ),

    {
      s('args_whole', t('$@')),
    },

    -- templates
    {
      s(
        'array_from_string',
        fmt(
          [[
            IFS='{delim}' read -ra {var} <<< "{str_split_by_ifs}"
          ]],
          {
            delim = i(1, ' '),
            var = i(2, 'var_name'),
            str_split_by_ifs = i(3, 'str_split_by_ifs'),
          }
        )
      ),

      s(
        'array_from_cmd_lines',
        fmt(
          [[
{}=()
while IFS='' read -r line ; do
  {}+=(\"$line\") ;
done < <({}){}]],
          {
            i(1, 'array'),
            i(2),
            i(3, 'cmd'),
            i(0),
          }
        )
      ),
    },

    sm(
      { 'read_lines_from_pipe', 'while_lines' },
      fmt(
        [[
while read -r {} ; do
  {}
done]],
        {
          i(1, 'line'),
          i(0),
        }
      )
    ),

    sm(
      { 'prompt', 'prompt_for_input', 'ask_for_input' },
      fmt([[read -rp '{}' {}]], {
        i(1, 'message'),
        i(0, 'var'),
      })
    ),

    sm(
      { 'which', 'command_v' },
      fmt([[command -v {} &> /dev/null]], {
        i(1, 'cmd'),
      })
    ),

    sm(
      { 'define_sourced_dir', 'define_this_script_dir' },
      fmt([[{}="$(cd -- "$(dirname -- "${{BASH_SOURCE:-\$0}}")" && pwd || exit 1)"]], {
        i(0, 'this_script_dir'),
      })
    ),

    {
      s(
        'assign_array',
        fmt('{}=("{}")', {
          i(1, 'new_array'),
          i(0, 'source_array'),
        })
      ),

      s(
        'export_function',
        fmt('export -f {}', {
          i(0, 'func_name'),
        })
      ),

      s(
        'poi',
        fmt([[echo "poi: ${{{}}}"]], {
          i(0, 'var_name'),
        })
      ),
    }
)
