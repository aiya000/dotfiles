---Neovimに関係がない、一般的なUtility関数を定義するモジュール

local Test = require('utils.test')

local M = {}

M.pipe = require('utils.pipe')
M.s = require('utils.functions.s').s
M.deep_equal = Test.deep_equal

---A try-catch-finally expression implementation
---
---@generic T
---@param f fun(): T --A function to run that may throws error
---@param catch fun(error_message: string): T --Cathces `do`'s error and returns `T`
---@param finally? fun() --A function that always called after `f` and `catch` called
---
---```lua
-----Simple usage like another languages:
---try(function()
---  -- May throw error
---end, function()
---  -- Catch error
---end, function()
---  -- Finally (always called, even if no error)
---end)
---
----- This result can be used as expression like Kotlin, Scala, and newer languages:
---local result = try(function()
---  foo() -- (May throw error)
---  return 'Hello, World!'
---end, function(err)
---  return 'Error: ' .. err
---end)  -- 'Hello, World!' or 'Error: ...'
---
----- If you have nothing to return in `f`, do `return nil`
---try(function()
---  -- ...
---  return nil
---end, function(err)
---  -- Some handling
---  return nil
---end)
---```
function M.try(f, catch, finally)
  local result = nil

  local ok, err = pcall(f)
  if ok then
    result = ok
  end
  if not ok then
    result = catch(err)
  end

  if finally ~= nil then
    finally()
  end
  return result
end

---Simular to `try()`, but doesn't catch error, only running `finally` after `f`
---@generic T
---@param f fun(): T
---@param finally fun()
---```lua
----- Throws error when `foo()` throws, but `finally()` always called
---try_finally(function()
---  foo() -- May throw error
---end, function()
---  -- Finally
---end)
---```
---@see M.try
function M.try_finally(f, finally)
  return M.try(f, function(err)
    error(err)
  end, finally)
end

---Quotes a string 'x' to `"'x'"` when string,
---makes table to string by `make_table_to_string()` when table,
---otherwise simply make to string by `tostring()`
---@param x unknown
---@return string
local function to_string_and_may_quote(x)
  return type(x) == 'string'
    and string.format("'%s'", x)
    or type(x) == 'table'
      and M.make_table_to_string(x)
      or tostring(x)
end

---Converts a table to a pretty string
---@param t table
---@return string
function M.make_table_to_string(t)
  local result = { '{' }

  for k, v in pairs(t) do
    local field = (' %s = %s,'):format(
      to_string_and_may_quote(k),
      to_string_and_may_quote(v)
    )
    table.insert(result, field)
  end

  table.insert(result, '}')
  return table.concat(result, '\n')
end

---Simular to `tostring()`, but with table support, like `vim.inspect()`.
---This is useful if you must do something when the fisrt schedule of Neovim (Meaning, when `vim.inspect()` cannot be used).
---@param x unknown
---@return string
function M.to_pretty_string(x)
  if type(x) == 'table' then
    return M.make_table_to_string(x)
  else
    return tostring(x)
  end
end

---Example:
---```lua
---local f = compose(
---  string.upper,
---  function(s) return s .. '!' end
---)
---f('hello')  -- 'HELLO!'
---```
function M.compose(...)
  local fs = { ... }
  return function(value)
    for i = 1, #fs do
      value = fs[i](value)
    end
    return value
  end
end

---@generic T
---@param x T
---@return T
function M.identity(x)
  return x
end

---@generic T
---@param x T
---@return fun(): T
function M.const(x)
  return function()
    return x
  end
end

---@generic T
---@param maybe_nil (T | nil) | (fun(): T | nil)
---@param default T
---@return T
function M.get_or_default(maybe_nil, default)
  local maybe_value = type(maybe_nil) == 'function' and maybe_nil() or maybe_nil

  if maybe_value == nil then
    return default
  else
    return maybe_value
  end
end

---Wait until p satisfied
---@param p fun(): boolean
---@param f fun(): nil --called when `p()` satisfied
---@param interval? integer --`p()` called every `interval` milliseconds. Defaults to 1000(ms)
---@return fun(): nil --A function to stop the timer
---Example:
---```lua
---local some_var = nil
---local stop_timer = wait_for(
---  function()
---    return some_var ~= nil
---  end,
---  function()
---    print('You got it!')
---  end
---)
----- Output: You got it!
----- (When `some_var` is set to a non-nil value by another thread)
---```
function M.wait_for(p, f, interval)
  local timer = vim.loop.new_timer()

  timer:start(0, interval or 1000, function()
    if p() then
      timer:stop()
      timer:close()
      vim.schedule(f)
    end
  end)

  return function()
    if not timer:is_closing() then
      timer:stop()
      timer:close()
    end
  end
end

---最初期のNeovimのスケジュールでvim.fn.trim()が使えないので、代わりに使う
---@param text string
function M.trim(text)
  return text:gsub('^%s*(.-)%s*$', '%1')
end

---Helper function to set a field in Vim dictionary variables.
---See `:h lua-vim-variables@nv` why this should be used.
---@param scope table --Expected: vim.g, vim.b, vim.w, vim.t, vim.v
---@param varname string --
---@param field string --
---@param value unknown --
---Example:
---```lua
---set_vim_dict_field(vim.g, 'my_config', 'debug_mode', true)
---set_vim_dict_field(vim.g, 'quickrun_config', 'ps1', {
---  command = 'powershell.exe',
---  exec = { '%c `wslpath -m %s`' },
---  tempfile = '%{tempname()}.ps1',
---})
---```
function M.set_vim_dict_field(scope, varname, field, value)
  if not scope[varname] then
    scope[varname] = {}
  end
  if type(scope[varname]) ~= 'table' then
    error(string.format("Expected '%s' to be a table, but got %s", varname, type(scope[varname])))
  end
  scope[varname][field] = value
end

-- In-source testing
if vim == nil then
  local test = Test.test
  local assert_equal = Test.assert_equal

  test('set_vim_dict_field() should set a sub field', function()
    local dict = {
      field = {},
    }
    M.set_vim_dict_field(dict, 'field', 'sub_field', 10)
    assert_equal(dict.field.sub_field, 10)
  end)

  test('set_vim_dict_field() should keep another field values', function()
    local dict = {
      another = 10,
      field = {},
    }
    M.set_vim_dict_field(dict, 'field', 'sub_field', 10)
    assert_equal(dict.another, 10)
  end)

  test('set_vim_dict_field() should set a sub field', function()
    local dict = {
      field = {},
    }
    M.set_vim_dict_field(dict, 'field', 'sub_field', 10)
    assert_equal(dict.field.sub_field, 10)
  end)

  test('set_vim_dict_field() should keep another field values', function()
    local dict = {
      another = 10,
      field = {},
    }
    M.set_vim_dict_field(dict, 'field', 'sub_field', 10)
    assert_equal(dict.another, 10)
  end)

  test('to_pretty_string() should convert primitives to string', function()
    assert_equal(M.to_pretty_string(42), '42')
    assert_equal(M.to_pretty_string('hello'), 'hello')
    assert_equal(M.to_pretty_string(true), 'true')
    assert_equal(M.to_pretty_string(false), 'false')
    assert_equal(M.to_pretty_string(nil), 'nil')
  end)

  test('to_pretty_string() should convert simple table to string', function()
    local result = M.to_pretty_string({ name = 'John', age = 30 })
    -- テーブルの順序は保証されないので、内容をチェック
    assert(result:find("'name' = 'John'"), 'Should contain name field with quotes')
    assert(result:find("'age' = 30"), 'Should contain age field')
    assert(result:find('^{'), 'Should start with {')
    assert(result:find('}$'), 'Should end with }')
  end)

  test('to_pretty_string() should handle nested tables', function()
    local nested = {
      user = { name = 'Alice' },
      active = true
    }
    local result = M.to_pretty_string(nested)
    assert(result:find("'user' = {"), 'Should contain nested table')
    assert(result:find("'name' = 'Alice'"), 'Should contain nested value with quotes')
    assert(result:find("'active' = true"), 'Should contain top-level value')
  end)

  test('to_pretty_string() should handle empty table', function()
    local result = M.to_pretty_string({})
    assert_equal(result, '{\n}')
  end)

  test('make_table_to_string() should work directly', function()
    local result = M.make_table_to_string({ key = 'value' })
    assert_equal(result, "{\n 'key' = 'value',\n}")
  end)

  test('make_table_to_string() should handle numeric keys', function()
    local result = M.make_table_to_string({ [1] = 'first', [2] = 'second' })
    assert(result:find("1 = 'first'") or result:find("2 = 'second'"), 'Should handle numeric keys with quoted strings')
  end)
end

return M
