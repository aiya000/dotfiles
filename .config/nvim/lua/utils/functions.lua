---Neovimに関係がない、一般的なUtility関数を定義するモジュール

local M = {}

M.pipe = require('utils.pipe')

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

---@param text string --テンプレートリテラルっぽい文字列
---@return string --変数埋め込み済み文字列
---Example:
---```lua
----- 変数の埋め込み
---local name = 'Alice'
---local age = 30
---local msg = s('Hello {name}! Next year you will be {age + 1}.')
---
----- （値の埋め込み）
---s('{10}')      -- '10'
---s('{5 + 3}')   -- '8'
---s('{math.pi}') -- '3.1415926535898'
---s('{"hello"}') -- 'hello'
---s('{true}')    -- 'true'
---s('{nil}')     -- 'nil'
---```
function M.s(text)
  -- 呼び出し元のローカル変数を取得
  local context = {}
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
  setmetatable(context, { __index = _G })

  local result = text:gsub('{([^}]+)}', function(expr)
    local code = 'return ' .. expr
    local f
    if _VERSION == 'Lua 5.1' and not jit then
      -- Lua 5.1 compatibility
      f = loadstring(code)
      if f then
        setfenv(f, context)
      end
    else
      -- LuaJIT/Lua 5.2+ (Neovim)
      f = load(code, nil, nil, context)
    end
    return f and tostring(f()) or '{' .. expr .. '}'
  end)
  return result
end

---@generic T
---@param x T
---@return T
function M.identity(x)
  return x
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
  local start_time = vim.loop.hrtime()

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
    error(string.format(
      "Expected '%s' to be a table, but got %s",
      varname, type(scope[varname])
    ))
  end
  scope[varname][field] = value
end

-- In-source testing
if vim == nil then
  local Test = require('utils.test')
  local test = Test.test
  local assert_equal = Test.assert_equal

  test('s() should return the taken string simply if with no embedded expressions', function()
    assert_equal(M.s('hi'), 'hi')
    assert_equal(M.s'hi', 'hi') -- Shorthand
  end)

  test('s() should embed variables', function()
    local name = 'aiya000'
    assert_equal(M.s'{name}', 'aiya000') -- A local variable
    assert_equal(M.s'{math.pi}', tostring(math.pi)) -- A global variable
  end)

  test('s() should embed values', function()
    assert_equal(M.s'{10}', '10')
    assert_equal(M.s'{5 + 3}', '8')
    assert_equal(M.s'{1.25}', '1.25')
    assert_equal(M.s'{"hello"}', 'hello')
    assert_equal(M.s'{nil}', 'nil') -- TODO: Fix
  end)

  test('s() should embed function and function call', function()
    local function f(x)
      return x
    end
    assert_equal(M.s'{f(10)}', '10')
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
