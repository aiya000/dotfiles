local ls = require('luasnip')
local s = ls.snippet
local sn = ls.snippet_node
local isn = ls.indent_snippet_node
local t = ls.text_node
local i = ls.insert_node
local f = ls.function_node
local c = ls.choice_node
local d = ls.dynamic_node
local r = ls.restore_node
local events = require('luasnip.util.events')
local ai = require('luasnip.nodes.absolute_indexer')
local extras = require('luasnip.extras')
local l = extras.lambda
local rep = extras.rep
local p = extras.partial
local m = extras.match
local n = extras.nonempty
local dl = extras.dynamic_lambda
local fmt = require('luasnip.extras.fmt').fmt
local fmta = require('luasnip.extras.fmt').fmta
local conds = require('luasnip.extras.expand_conditions')
local postfix = require('luasnip.extras.postfix').postfix
local types = require('luasnip.util.types')
local parse = require('luasnip.util.parser').parse_snippet
local ms = ls.multi_snippet
local k = require('luasnip.nodes.key_indexer').new_key

local sm = function(triggers, snippet_def)
  return ms(triggers, { snippet_def })
end

return {
  -- Method definition
  sm(
    { 'def', 'fun' },
    s(
      'def',
      fmt('def {}{}: {} = {}', {
        i(1, 'funcName'),
        i(2),
        i(3, 'Type'),
        i(0),
      })
    )
  ),

  -- Object definition
  s(
    'object',
    fmt('object {}', {
      i(1, 'ObjectName'),
    })
  ),

  -- Package declaration
  s(
    'package',
    fmt('package {}', {
      i(0, 'packagename'),
    })
  ),

  -- Sealed abstract class
  s(
    'sealed_abstract_class',
    fmt('sealed abstract class {}', {
      i(1, 'TypeName'),
    })
  ),

  -- Sealed trait
  sm(
    { 'sealed_trait', 'sum_type_parent' },
    s(
      'sealed_trait',
      fmt('sealed trait {}', {
        i(1, 'TypeName'),
      })
    )
  ),

  -- Case object
  sm(
    { 'case_object', 'sum_type_chlild' },
    s(
      'case_object',
      fmt('case object {} extends {}', {
        i(1, 'ValueName'),
        i(2, 'ParentName'),
      })
    )
  ),

  -- Case class
  sm(
    { 'case_class', 'product_type' },
    s(
      'case_class',
      fmt('case class {}({})', {
        i(1, 'TypeName'),
        i(0, 'args'),
      })
    )
  ),

  -- Class definition
  s(
    'class',
    fmt('class {}{} {}', {
      i(1, 'TypeName'),
      i(2),
      i(0),
    })
  ),

  -- Import statement
  s(
    'import',
    fmt('import {}', {
      i(0),
    })
  ),

  -- Package object
  s(
    'package_object',
    fmt(
      [[
    /**
     * {}
     */
    package object {}
  ]],
      {
        i(1),
        i(0, 'packagename'),
      }
    )
  ),

  -- Match expression
  s(
    'match',
    fmt(
      [[
    match {{
        case {} => {}
    }}
  ]],
      {
        i(1, 'Pattern'),
        i(0),
      }
    )
  ),

  -- Case clause
  s(
    'case',
    fmt('case {} => {}', {
      i(1, 'Pattern'),
      i(0),
    })
  ),

  -- Throw statement
  s(
    'throw',
    fmt('throw {}', {
      i(0),
    })
  ),

  -- Try block
  s(
    'try',
    fmt('try {{{}}}', {
      i(0),
    })
  ),

  -- Catch block
  s(
    'catch',
    fmt(
      [[
    catch {{
        case {}: {} => {}
    }}
  ]],
      {
        i(1, 'varName'),
        i(2, 'Exception'),
        i(0),
      }
    )
  ),

  -- Finally block
  s(
    'finally',
    fmt('finally {{{}}}', {
      i(0),
    })
  ),

  -- Type alias
  sm(
    { 'type', 'type_alias' },
    s(
      'type',
      fmt('type {} = {}', {
        i(1, 'TypeName'),
        i(0, 'OriginalName'),
      })
    )
  ),

  -- Implicit keyword
  s(
    'implicit',
    fmt('implicit {}', {
      i(0),
    })
  ),

  -- If statement
  s(
    'if',
    fmt('if ({}) {{{}}}', {
      i(1, 'cond'),
      i(0, 'here'),
    })
  ),

  -- Else if statement
  s(
    'else_if',
    fmt('else if ({}) {{{}}}', {
      i(1, 'cond'),
      i(0, 'here'),
    })
  ),

  -- Else statement
  s(
    'else',
    fmt('else {{{}}}', {
      i(0, 'here'),
    })
  ),

  -- For comprehension
  s(
    'for',
    fmt('for {{{}}}', {
      i(0, 'here'),
    })
  ),

  -- While loop
  s(
    'while',
    fmt('while ({}) {{{}}}', {
      i(1, 'cond'),
      i(0, 'here'),
    })
  ),

  -- Yield keyword
  s('yield', t('yield')),

  -- Private keyword
  s('private', t('private')),

  -- Final keyword
  s('final', t('final')),

  -- Override keyword
  s('override', t('override')),

  -- Macro keyword
  s('macro', t('macro')),

  -- Extend list to varargs
  s('extend_list_to_varargs', t(':_*')),

  -- Println
  s(
    'println',
    fmt('println({})', {
      i(0, 'here'),
    })
  ),

  -- Println to stderr
  s(
    'println_err',
    fmt('System.err.println({})', {
      i(0, 'her'),
    })
  ),

  -- TODO placeholder
  sm(
    { 'throw_runtime_exception_as_todo_not_implemented_yet', 'todo', 'undefined', 'not_implemented_yet' },
    s('todo', t('???'))
  ),

  -- Fatal error
  sm(
    { 'throw_runtime_exception_as_a_fatal_errror', 'fatal_errror' },
    s(
      'fatal_error',
      fmt('throw new RuntimeException("Fatal error! {}")', {
        i(0),
      })
    )
  ),

  -- Quasi quote
  sm(
    { 'quasi_quote', 'q' },
    s(
      'quasi_quote',
      fmt('q"""{}"""', {
        i(0),
      })
    )
  ),

  -- Implicit parameters
  s(
    'impliclit_parameters_on_a_method',
    fmt('(implicit {}: {}{})', {
      i(1, 'var'),
      i(2, 'Type'),
      i(3),
    })
  ),

  -- Sum type template
  s(
    'sum_type',
    fmt(
      [[
    sealed trait {}

    object trait {} {{
        case class {}({}) extends {}
    }}
  ]],
      {
        i(1, 'TypeName'),
        rep(1),
        i(2, 'ValueName'),
        i(3, 'args'),
        rep(1),
      }
    )
  ),

  -- Implicit class (extension method)
  sm(
    { 'implicit_class', 'extension_method', 'enrich_my_library' },
    s(
      'implicit_class',
      fmt(
        [[
    implicit class {}({}: {}) {{
        def {}({}): {} = {}
    }}
  ]],
        {
          i(1, 'AlternativeName'),
          i(2, 'self'),
          i(3, 'TargetType'),
          i(4, 'funcName'),
          i(5),
          i(6, 'Type'),
          i(0),
        }
      )
    )
  ),

  -- Type reflection template
  s(
    'template_getType',
    fmt(
      [[
    import scala.reflect.runtime.{{universe => ru}}
    import ru._
    def getType[A : TypeTag](x: A): Type = typeOf[A]
  ]],
      {}
    )
  ),

  -- Multiline string with margin
  s(
    'template_multiline_string_with_margin',
    fmt(
      [[
    """
    | {}
    """.trim.stripMargin
  ]],
      {
        i(0),
      }
    )
  ),

  -- Arrow (=>)
  sm({ 'to', 'arrow' }, s('to', t('=>'))),

  -- For comprehension arrow (<-)
  sm({ 'from', 'for_into', 'into' }, s('from', t('<-'))),

  -- Main method template
  s(
    'main',
    fmt(
      [[
    object Main {{
        def main(args: Array[String]): Unit = {{
            {}
        }}
    }}
  ]],
      {
        i(0),
      }
    )
  ),

  -- Instance check
  s(
    'isInstanceOf',
    fmt('isInstanceOf[{}]', {
      i(1, 'TypeName'),
    })
  ),

  -- Debug pattern (let it)
  s(
    'let_it',
    fmt(
      [[
    match {{
        case it => {}; it
    }}
  ]],
      {
        i(1, 'println(it)'),
      }
    )
  ),
}
