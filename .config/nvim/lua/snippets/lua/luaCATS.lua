local fmt = require('luasnip.extras.fmt').fmt
local list = require('utils.list')
local ls = require('luasnip')
local utils = require('utils.luasnip')

local sm = utils.sm
local s = ls.snippet
local i = ls.insert_node

return list.concat(
  sm({'document_comment', 'doc'}, fmt([[
    ---
  ]], {})),

  {
    s('param', fmt([[
      ---@param {var} {type}{description}
    ]], {
      var = i(1, 'var'),
      type = i(2, 'type'),
      description = i(3, ''),
    })),

    s('return', fmt([[
      ---@return {type}{description}
    ]], {
      type = i(1, 'type'),
      description = i(2, ''),
    })),
  },

  sm({'alias', 'type_alias'}, fmt([[
    ---@alias {NewName} {type}
  ]], {
    NewName = i(1, 'NewName'),
    type = i(2, 'type'),
  })),

  sm({'define_class_type', 'defclass'}, fmt([[
    ---@class {ClassName}
    ---@field {prop_name} {type}
  ]], {
    ClassName = i(1, 'ClassName'),
    prop_name = i(2, 'prop_name'),
    type = i(3, 'type'),
  })),

  {
    s('class_type', fmt([[
      ---@class {ClassName}
    ]], {
      ClassName = i(1, 'ClassName'),
    })),

    s('field', fmt([[
      ---@field {field_name} {type}
    ]], {
      field_name = i(1, 'field_name'),
      type = i(2, 'type'),
    })),

    s('generic', fmt([[
      ---@generic {TypeArgName}
    ]], {
      TypeArgName = i(1, 'TypeArgName'),
    })),

    s('overload', fmt([[
      ---@overload fun({args}): {return_type}
    ]], {
      args = i(1, 'args'),
      return_type = i(2, 'return_type'),
    })),

    s('type', fmt([[
      ---@type {VarType}
    ]], {
      VarType = i(1, 'VarType'),
    })),

    s('see', fmt([[
      ---@see {symbol}
    ]], {
      symbol = i(1, 'symbol'),
    })),

    s('as', fmt([=[
      --[[@as {CoercedType}]]
    ]=], {
      CoercedType = i(1, 'CoercedType'),
    })),
  },

  sm({'array_as', 'as_of_array'}, fmt([==[
    --[=[@as {ElementType}[]]=]
  ]==], {
    ElementType = i(1, 'ElementType'),
  })),

  {
    s('type_string', fmt([[
      string
    ]], {})),

    s('type_number', fmt([[
      number
    ]], {})),
  },

  sm({'type_integer', 'type_int', 'int'}, fmt([[
    integer
  ]], {})),

  {
    s('type_boolean', fmt([[
      boolean
    ]], {})),
  },

  sm({'type_any_table', 'type_table_any'}, fmt([[
    table
  ]], {})),

  {
    s('type_function', fmt([[
      function
    ]], {})),

    s('type_any', fmt([[
      any
    ]], {})),
  },

  sm({'type_unknown', 'unknown', 'unk'}, fmt([[
    unknown
  ]], {})),

  {
    s('type_nil', fmt([[
      nil
    ]], {})),

    s('type_void', fmt([[
      nil
    ]], {})),
  },

  sm({'type_fun', 'tfun'}, fmt([[
    fun({args}): {type}
  ]], {
    args = i(1, 'args'),
    type = i(2, 'type'),
  })),

  {
    s('type_array', fmt([[
      {type}[]
    ]], {
      type = i(1, 'type'),
    })),

    s('type_table', fmt([[
      table<{KeyType}, {ValueType}>
    ]], {
      KeyType = i(1, 'KeyType'),
      ValueType = i(2, 'ValueType'),
    })),

    s('type_object', fmt([[
      {{ {prop}: {type} }}
    ]], {
      prop = i(1, 'prop'),
      type = i(2, 'type'),
    })),

    s('type_tuple', fmt([[
      [{types}]
    ]], {
      types = i(1, 'types'),
    })),

    s('type_union', fmt([[
      {type1} | {type2}
    ]], {
      type1 = i(1, 'type1'),
      type2 = i(2, 'type2'),
    })),

    s('type_intersection', fmt([[
      {type1} & {type2}
    ]], {
      type1 = i(1, 'type1'),
      type2 = i(2, 'type2'),
    })),

    s('type_optional', fmt([[
      {type}?
    ]], {
      type = i(1, 'type'),
    })),

    s('type_varargs', fmt([[
      ...{type}
    ]], {
      type = i(1, 'type'),
    })),
  },

  sm({'example_comment', 'example'}, fmt([[
    ---Example:
    ---```lua
    ---{}
    ---```
  ]], {
    i(1, ''),
  }))
)