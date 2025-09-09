local M = {}

---@see s
---For Lua 5.1 compatibility
---@param ref { code: string, context: table }
---@return string --The interplated string
local function replace_expr_in_context_for_lua_5_1_non_jit(ref)
  local f = loadstring(ref.code)
  if f == nil then
    error('Failed to compile expression: ' .. ref.expr)
  end
  setfenv(f, ref.context)

  local ok, result = pcall(f)
  if not ok then
    error('Error evaluating expression: ' .. ref.expr .. ' - ' .. tostring(result))
  end

  return tostring(result)
end

---This function is same as `replace_expr_in_context_for_lua_5_1_non_jit()`,
---but for LuaJIT/Lua 5.2+ (Neovim).
---@see s
---@see replace_expr_in_context_for_lua_5_1_non_jit
local function replace_expr_in_context(ref)
  local f = load(ref.code, nil, nil, ref.context)
  if f == nil then
    error('Failed to compile expression: ' .. ref.expr)
  end

  local ok, result = pcall(f)
  if not ok then
    error('Error evaluating expression: ' .. ref.expr .. ' - ' .. tostring(result))
  end

  return tostring(result)
end

---@param text string --テンプレートリテラル文字列
---@param vars table --変数テーブル（必須）
---@return string --変数埋め込み済み文字列
---
---Example:
---```lua
----- 変数の埋め込み
---local msg = s('Hello {name}! Next year you will be {age + 1}.', {
---  name = 'Alice',
---  age = 19,
---})
---
----- 複数回の変数の使用
---local x = s('Hello, {foo}! {foo}', { foo = 'world' })
---
----- 式の評価
---s('{x + y}', { x = 5, y = 3 })      -- '8'
---s('{math.pi}', {})                  -- '3.1415926535898' (グローバル変数)
---s('{name or "Unknown"}', { name = nil }) -- 'Unknown'
---```
function M.s(text, vars)
  if type(vars) ~= 'table' then
    error('s() requires a variable table as the second argument')
  end

  setmetatable(vars, {
    __index = function(_, key)
      local global_value = _G[key]
      if global_value ~= nil then
        return global_value
      end
      error('undefined variable: ' .. tostring(key))
    end
  })

  return text:gsub('{([^}]+)}', function(expr)
    local replacing_ref = {
      code = 'return ' .. expr,
      context = vars,
      expr = expr,
    }
    return (
      (_VERSION == 'Lua 5.1' and not jit) and replace_expr_in_context_for_lua_5_1_non_jit
      or replace_expr_in_context
    )(replacing_ref)
  end)
end

-- In-source testing
if vim == nil then
  local Test = require('utils.test')
  local test = Test.test
  local assert_equal = Test.assert_equal
  local assert_to_throw = Test.assert_to_throw

  test('s() should return the taken string simply if with no embedded expressions', function()
    assert_equal(M.s('hi', {}), 'hi')
    assert_equal(M.s('hello world', {}), 'hello world')
  end)

  test('s() should embed variables', function()
    assert_equal(M.s('Hello {name}', { name = 'Konoko' }), 'Hello Konoko')
  end)

  test('s() should embed values', function()
    assert_equal(M.s('{10}', {}), '10')
    assert_equal(M.s('{5 + 3}', {}), '8')
    assert_equal(M.s('{1.25}', {}), '1.25')
    assert_equal(M.s('{"hello"}', {}), 'hello')
    assert_equal(M.s('{nil}', {}), 'nil')
  end)

  test('s() should embed function and function call', function()
    local function f(x)
      return x
    end
    assert_equal(M.s('{f(10)}', { f = f }), '10')
  end)

  test('s() should be error if unknown variable is embedded', function()
    assert_to_throw(function()
      M.s('{nonexistent_var}', {})
    end)
  end)

  test('s() should use variable table values', function()
    assert_equal(M.s('{name}', { name = 'Konoko' }), 'Konoko')
  end)

  test('s() should work in nested contexts', function()
    local function level1()
      local function level2()
        return M.s('{x}', { x = 10 })
      end
      return level2()
    end
    assert_equal(level1(), '10')
  end)
end

return M
