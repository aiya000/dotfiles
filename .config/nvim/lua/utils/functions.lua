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

  return text:gsub('{([^}]+)}', function(expr)
    local f = load('return ' .. expr, nil, nil, context)
    return f and f() or '{' .. expr .. '}'
  end)
end

---@generic T
---@param x T
---@return T
function M.identity(x)
  return x
end

-- In-source testing
if vim == nil then
  ---@param description string
  ---@param check fun(): nil
  ---Example:
  ---```lua
  ----- No output
  ---test('X should Y', function()
  ---  -- No errors
  ---end);
  ---
  ----- - Failed: X should Y
  -----   X could not Y
  ---test('X should Y', function()
  ---  error('X could not Y')
  ---end);
  ---```
  local function test(description, check)
    local ok, maybe_error_message = pcall(check)
    if not ok then
      print('- Failed: ' .. description)
      print('  ' .. maybe_error_message)
    end
  end

  ---@generic T
  ---@param actual T
  ---@param expected T
  ---@return nil --Throws an error message if `actual` does not equal `expected`
  ---Example:
  ---```lua
  ---assert_equal(1 + 1, 2) -- No error
  ---assert_equal(1 + 1, 3) -- Expected: 3, but got: 2
  ---
  ----- - Failed: `1 + 1` should be `3`
  -----   Expected: 3, but got: 2
  ---test('`1 + 1` should be `3`', function()
  ---  assert_equal(1 + 1, 3)
  ---end);
  ---```
  local function assert_equal(actual, expected)
    if actual ~= expected then
      error(string.format('Expected: %s, but got: %s', tostring(expected), tostring(actual)))
    end
  end

  test('s() should return the taken string simply if with no embedded expressions', function()
    assert_equal(M.s('hi'), 'hi')
  end)

  test('s() should embed variables', function()
    local name = 'aiya000'
    assert_equal(M.s('{name}'), 'aiya000')
  end)

  test('s() should embed values', function()
    assert_equal(M.s('{10}'), '10')
    assert_equal(M.s('{5 + 3}'), '8')
    assert_equal(M.s('{1.25}'), '1.25')
    assert_equal(M.s('{"hello"}'), 'hello')
    assert_equal(M.s('{nil}'), 'nil') -- TODO: Fix
  end)
end

return M
