--- Testing functions and several another functions (to undepend another modules)

local M = {}

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
function M.test(description, check)
  local ok, maybe_error_message = pcall(check)
  if not ok then
    print('- Failed: ' .. description)
    print('  ' .. maybe_error_message)
  end
end

---@generic T, U
---@param array T[]
---@param reducer fun(acc: U, item: T, index: integer): U
---@param initial U
---@return U
function M.reduce(array, reducer, initial)
  local acc = initial
  for i = 1, #array do
    acc = reducer(acc, array[i], i)
  end
  return acc
end

---table.concat()とほぼ同じ。
---ただしtable.concat()はnilを無視するが、これは無視をしない。
---
---@param xs unknown[]
---@return string
function M.concat_array_including_nil(xs)
  return M.reduce(xs, function(str, new)
    return str .. tostring(new)
  end, '')
end

---配列の要素としての見た目に変換する
---@param x unknown
---@return string
function M.to_element_string(x)
  return type(x) == 'string'
    and string.format("'%s'", x)
    or tostring(x)
end

---Makes the taken array to a pretty string
---
---@param xs unknown[]
---@return string
---
---Warning:
---```lua
----- The trailing nil is omitted because of Lua's specification
---local result = make_array_to_string({1, 2, nil})
---print(result) -- { 1, 2 }
---```
function M.make_array_to_string(xs)
  -- nilを含む配列の長さを正しく取得
  local max_index = 0
  for i, _ in pairs(xs) do
    if type(i) == 'number' and i > max_index then
      max_index = i
    end
  end

  local result = {'{ '}
  for i = 1, max_index do
    table.insert(result, M.to_element_string(xs[i]))
    if i < max_index then
      table.insert(result, ', ')
    end
  end
  return M.concat_array_including_nil(result) .. ' }'
end

---@param xs table | unknown[]
---@return boolean
function M.is_array(xs)
  if type(xs) ~= 'table' then
    return false
  end

  local count = 0
  for k, _ in pairs(xs) do
    if type(k) ~= 'number' then
      return false
    end
    count = count + 1
  end

  return count == #xs
end

-- TODO: 通常のテーブルも表示する
---tostring()とほぼ同じ。
---ただし、配列を表示する。
---@param x unknown
---@return string
function M.to_pretty_string(x)
  if M.is_array(x) then
    return M.make_array_to_string(x)
  else
    return tostring(x)
  end
end

---Checks two values are deeply equal
---
---@generic T
---@param actual T
---@param expected T
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

function M.assert_equal(actual, expected)
  if not M.deep_equal(actual, expected) then
    error(string.format('Expected: %s, but got: %s', M.to_pretty_string(expected), M.to_pretty_string(actual)))
  end
  return true
end

---@param f function
---@return true | nil --Returns true if assertion is succeed. Throws an error message if `actual` does not equal `expected`
function M.assert_to_throw(f)
  local ok = pcall(f)
  if ok then
    error('Expected a function to throw an error, but it did not.')
  end
  return true
end

-- In-source testing
if vim == nil then
  -- Test deep_equal
  if not M.deep_equal(1, 1) then
    error('1 should equal 1')
  end
  if M.deep_equal(1, 2) then
    error('1 should not equal 2')
  end

  if not M.deep_equal('hello', 'hello') then
    error("'hello' should equal 'hello'")
  end
  if M.deep_equal('hello', 'world') then
    error("'hello' should not equal 'world'")
  end

  if not M.deep_equal(true, true) then
    error('true should equal true')
  end
  if M.deep_equal(true, false) then
    error('true should not equal false')
  end

  if not M.deep_equal(nil, nil) then
    error('nil should equal nil')
  end
  if M.deep_equal(1, nil) then
    error('1 should not equal nil')
  end
  if M.deep_equal(nil, 2) then
    error('nil should not equal 2')
  end

  if not M.deep_equal({a = 1}, {a = 1}) then
    error('{a = 1} should equal {a = 1}')
  end
  if M.deep_equal({a = 1}, {a = 2}) then
    error('{a = 1} should not equal {a = 2}')
  end
  if not M.deep_equal({a = 1, b = {c = 2}}, {a = 1, b = {c = 2}}) then
    error('{a = 1, b = {c = 2}} should equal {a = 1, b = {c = 2}}')
  end

  if not M.deep_equal({1, 2, 3}, {1, 2, 3}) then
    error('{1, 2, 3} should equal {1, 2, 3}')
  end
  if M.deep_equal({1, 2, 3}, {10, 20, 30}) then
    error('{1, 2, 3} should not equal {10, 20, 30}')
  end

  -- Test assert_equal
  if not M.assert_equal(1 + 1, 2) then
    error('1 + 1 should be 2')
  end

  local ok, result = pcall(M.assert_equal, 1 + 1, 3)
  if ok then
    error('1 + 1 should not be 3')
    if result ~= 'Expected: 3, but got: 2' then
      error('Unexpected error message: ' .. result)
    end
  end

  -- Test assert_to_throw
  local function to_throw()
    error('error')
  end
  local ok = pcall(M.assert_to_throw, to_throw)
  if not ok then
    error('to_throw() should throw an error')
  end

  local function not_to_throw()
    return 10
  end
  local ok = pcall(M.assert_to_throw, not_to_throw)
  if ok then
    error('not_to_throw() should throw an error')
  end

  local test = M.test
  local assert_equal = M.assert_equal

  test('reduce() should reduce an array to a single value', function()
    local sum = M.reduce({1, 2, 3, 4}, function(acc, item)
      return acc + item
    end, 0)
    assert_equal(sum, 10)

    local str = M.reduce({'a', 'b', 'c'}, function(acc, item)
      return acc .. item
    end, '')
    assert_equal(str, 'abc')
  end)

  test('reduce() should return the initial value if the empty array is taken', function()
    local result = M.reduce({}, function(acc, item)
      error('error')
    end, 'result')
    assert_equal(result, 'result')
  end)

  test('concat_array_including_nil() should concat an array including nil', function()
    assert_equal(M.concat_array_including_nil({1, ', ', nil, ', ', 'a'}), '1, nil, a')
    assert_equal(M.concat_array_including_nil({}), '')
  end)

  test('to_element_string() should make values to the string of an array\'s element', function()
    assert_equal(M.to_element_string('hello'), "'hello'")
    assert_equal(M.to_element_string(10), '10')
    assert_equal(M.to_element_string(true), 'true')
    assert_equal(M.to_element_string(nil), 'nil')
  end)

  test('make_array_to_string() should make array to string', function()
    assert_equal(M.make_array_to_string({1, 2, 3}), '{ 1, 2, 3 }')
    assert_equal(M.make_array_to_string({1, nil, 3}), "{ 1, nil, 3 }")
    assert_equal(M.make_array_to_string({1, 'a', true}), "{ 1, 'a', true }")
    assert_equal(M.make_array_to_string({}), '{  }')
  end)

  test('make_array_to_string() cannot include trailing nil because Lua\'s specification', function()
    assert_equal(M.make_array_to_string({1, 2, nil}), "{ 1, 2 }")
  end)

  test('is_array() should check if a table is an array', function()
    assert_equal(M.is_array({1, 2, 3}), true)
    assert_equal(M.is_array({1, 'a', true, nil}), true)
    assert_equal(M.is_array({}), true)

    assert_equal(M.is_array({a = 1, b = 2}), false)
    assert_equal(M.is_array(10), false)
    assert_equal(M.is_array(nil), false)
  end)

  test('to_pretty_string() should convert to a pretty string', function()
    assert_equal(M.to_pretty_string({1, 2, 3}), '{ 1, 2, 3 }')
    assert_equal(M.to_pretty_string({'a', 'b', 'c'}), "{ 'a', 'b', 'c' }")
    assert_equal(M.to_pretty_string(10), '10')
  end)
end

return M
