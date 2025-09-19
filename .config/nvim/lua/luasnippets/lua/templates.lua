local fmt = require('luasnip.extras.fmt').fmt
local list = require('utils.list')
local ls = require('luasnip')
local utils = require('utils.luasnip')

local sm = utils.sm
local s = ls.snippet
local i = ls.insert_node

return list.concat(
  sm({'try_catch', 'try', 'catch'}, fmt([[
    local ok, {result} = pcall({func_name}{args})
    if not ok then
      {}
    end
  ]], {
    result = i(3, 'result'),
    func_name = i(1, 'func_name'),
    args = i(2, ''),
    i(4, 'handling'),
  })),

  sm({'define_module', 'module'}, fmt([[
    local M = {{}}
    return M
  ]], {})),

  sm({'import', 'imp'}, fmt([[
    local {name} = require('{module}')
  ]], {
    name = i(1, 'name'),
    module = i(2, 'module'),
  })),

  sm({'define_print_table', 'define_print_object', 'define_print_dict'}, fmt([[
    local function print_table(t)
      print('{{')
      for k, v in pairs(t) do
        print('  ' .. k, '=', v .. ',')
      end
      print('}}')
    end
  ]], {})),

  sm({'print_table', 'prt', 'pt'}, fmt([[
    {module}print_table({var})
  ]], {
    module = i(1, 'module.'),
    var = i(2, 'var'),
  })),

  {
    s('define_pipe', fmt([[
      local function pipe(value)
        return {{
          value = value,
          let = function(self, f)
            return pipe(f(self.value))
          end,
          get = function(self)
            return self.value
          end
        }}
      end
    ]], {})),

    s('pipe', fmt([[
      {module}pipe({expr})
    ]], {
      module = i(1, 'module.'),
      expr = i(2, 'expr'),
    })),

    s('define_compose', fmt([[
      local function compose(...)
        local fs = {{...}}
        return function(value)
          for i = 1, #fs do
            value = fs[i](value)
          end
          return value
        end
      end
    ]], {})),
  },

  sm({'compose', 'comp'}, fmt([[
    {module}compose({functions})
  ]], {
    module = i(1, 'module.'),
    functions = i(2, 'functions'),
  })),

  sm({'define_s', 'define_template_string'}, fmt([[
    local function s(text)
      -- 呼び出し元のローカル変数を取得
      local context = {{}}
      local level = 2
      local i = 1
      while true do
        local name, value = debug.getlocal(level, i)
        if not name then break end
        if not name:match('^%(') then -- 一時変数を除外
          context[name] = value
        end
        i = i + 1
      end

      -- グローバル変数を取得
      setmetatable(context, {{ __index = _G }})

      return text:gsub('{{([^}}]+)}}', function(expr)
        local f = load('return ' .. expr, nil, nil, context)
        return f and tostring(f()) or '{{' .. expr .. '}}'
      end)
    end
  ]], {})),

  sm({'s', 'template_string'}, fmt([[
    s'{text}'
  ]], {
    text = i(1, 'text'),
  })),

  sm({'class', 'cla'}, fmt([[
    ---@class {ClassName}
    local {ClassName} = {{}}
    {ClassName}.__index = {ClassName}

    ---@return {ClassName}
    function {ClassName}.new({constructor_args})
      local self = setmetatable({{}}, {ClassName})
      self.{arg} = {arg}
      return self
    end
  ]], {
    ClassName = i(1, 'ClassName'),
    constructor_args = i(2, 'constructor_args'),
    arg = i(3, 'arg'),
  })),

  sm({'method', 'met'}, fmt([[
    function {class}:{method_name}()
      {}
    end
  ]], {
    class = i(1, 'class'),
    method_name = i(2, 'method_name'),
    i(3, ''),
  })),

  {
    s('call_lambda', fmt([[
      (function {f}({args})
         {}
      end)({args})
    ]], {
      f = i(1, 'f'),
      args = i(2, 'args'),
      i(3, ''),
    })),
  },

  sm({'define_in_source_test_for_neovim', 'in_source_test'}, fmt([[
    -- In-source testing
    if vim == nil then
      local Test = require('utils.test')
      local test = Test.test
      local assert_equal = Test.assert_equal

      {}
    end
  ]], {
    i(1, ''),
  })),

  {
    s('in_source_test_without_helpers', fmt([[
      -- In-source testing
      if vim == nil then
        {}
      end
    ]], {
      i(1, ''),
    })),
  },

  sm({'define_test', 'deftest'}, fmt([[
    test('{tested_function}() should {what_should_be_done}', function()
      {}
    end)
  ]], {
    tested_function = i(1, 'tested_function'),
    what_should_be_done = i(2, 'what_should_be_done'),
    i(3, ''),
  })),

  {
    s('todo', fmt([[
      print('TODO: Not Implemented Yet ({function_name})')
    ]], {
      function_name = i(1, 'function_name'),
    })),

    s('poi', fmt([[
      vim.notify('poi: ' .. vim.inspect({}), vim.log.levels.INFO)
    ]], {
      i(1, ''),
    }))
  }
)