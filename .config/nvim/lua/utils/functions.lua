---Neovimに関係がない、一般的なUtility関数を定義するモジュール

local Test = require('utils.test')

local M = {}

M.pipe = require('utils.pipe')
M.s = require('utils.functions.s').s
M.deep_equal = Test.deep_equal

-- TODO: Recursively
function M.print_table(t)
  print('{')
  for k, v in pairs(t) do
    print('  ' .. k, '=', v .. ',')
  end
  print('}')
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
end

return M
