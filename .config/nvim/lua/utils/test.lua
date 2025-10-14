--- Testing functions and several another functions (to undepend another modules)

local M = {}

---@param description string
---@param check fun(): nil
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

---TODO: `initial` argument to first argument
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
  return type(x) == 'string' and string.format("'%s'", x) or tostring(x)
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

  local result = { '{ ' }
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



return M
