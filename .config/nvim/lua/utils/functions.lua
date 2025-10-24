---Neovimに関係がない、一般的なUtility関数を定義するモジュール

local M = {}

M.pipe = require('utils.pipe')
M.s = require('utils.functions.s').s

---Alternative of vim.deep_equal().
---Checks two values are deeply equal.
---
---@generic T
---@param a T
---@param b T
---@return boolean
---
---Example:
---```lua
---deep_equal(1, 1)          -- true
---deep_equal(1, 2)          -- false
---
---deep_equal('hello', 'hello') -- true
---deep_equal('hello', 'world') -- false
---
---deep_equal(true, true)    -- true
---deep_equal(true, false)   -- false
---
---deep_equal(nil, nil)      -- true
---deep_equal(1, nil)       -- false
---deep_equal(nil, 2)       -- false
---
---deep_equal({a = 1}, {a = 1}) -- true
---deep_equal({a = 1}, {a = 2}) -- false
---deep_equal({a = 1, b = {c = 2}}, {a = 1, b = {c = 2}}) -- true
---
---deep_equal({1, 2, 3}, {1, 2, 3}) -- true
---deep_equal({1, 2, 3}, {10, 20, 30}) -- false
---```
---
function M.deep_equal(a, b)
  if type(a) ~= type(b) then
    return false
  end
  if type(a) ~= 'table' then
    return a == b
  end

  -- Check if both tables have the same keys and values
  for key, value in pairs(a) do
    if not M.deep_equal(value, b[key]) then
      return false
    end
  end
  for key, _ in pairs(b) do
    if a[key] == nil then
      return false
    end
  end
  return true
end

---A try-catch-finally expression implementation
---@generic T
---@param f fun(): T --A function to run that may throws error
---@param catch fun(error_message: string): T --Cathces `do`'s error and returns `T`
---@param finally? fun() --A function that always called after `f` and `catch` called
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

-- TODO: Write tests
---Creates a readonly table
---@generic K, V
---@param x table<K, V>
---@return table<K, V> --But readonly
---
---raedonly() is lightweight than deep_readonly()
---@see deep_readonly
---
---```lua
----- OK - Flat tables can be readonly
---local x = readonly({
---  a = 10,
---})
---local ok1 = pcall(function()
---  x.a = 20
---end)
---print(not ok1) -- true
---
----- NG - Nested tables doesn't deny assignment to its fields
---local y = readonly({
---  a = {
---    b = 10,
---  },
---})
---local ok2 = pcall(function()
---  y.a.b = 20
---end)
---print(not ok2) -- false
---```
function M.readonly(x)
  return setmetatable({}, {
    __index = x,
    __newindex = function(_, key, value)
      error(('The table is readonly. { key = %s, value = %s }'):format(key, M.to_pretty_string(value)))
    end,
    __metatable = false
  })
end

-- TODO: Write tests
---Creates a deeply readonly table
---@generic K, V
---@param x table<K, V>
---@return table<K, V> --But deeply readonly
---
---deep_raedonly() is heavy than readonly()
---@see readonly
---
---```lua
----- OK - Flat tables can be readonly
---local x = deep_readonly({
---  a = 10,
---})
---local ok1, result1 = pcall(function()
---  x.a = 20
---end)
---print(not ok1) -- true
---
----- OK - Nested tables are also readonly
---local y = deep_readonly({
---  a = {
---    b = 10,
---  },
---})
---local ok2, result2 = pcall(function()
---  y.a.b = 20
---end)
---print(not ok2) -- true
---```
function M.deep_readonly(x)
  ---@generic K, V
  ---@param a table<K, V>
  ---@return table<K, V>
  local function create_nested_deep_readonly(a)
    return setmetatable({}, {
      __index = M.deep_readonly(a),
      __newindex = function(_, key, value)
        error(('The table is readonly. { key = %s, value = %s }'):format(key, M.to_pretty_string(value)))
      end,
      __metatable = false
    })
  end

  local result = {}
  for key, value in pairs(x) do
    if type(value) == 'table' then
      result[key] = create_nested_deep_readonly(value)
    else
      result[key] = value
    end
  end
  return M.readonly(result)
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
  if timer == nil then
    error('Failed to create a new timer')
  end

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

return M
