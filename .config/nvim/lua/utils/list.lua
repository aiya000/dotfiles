-- TODO: Rename to array.lua

local Test = require('utils.test')

local M = {}

M.reduce = Test.reduce
M.concat_array_including_nil = Test.concat_array_including_nil
M.to_element_string = Test.to_element_string
M.make_array_to_string = Test.make_array_to_string
M.is_array = Test.is_array

---Checks two lists are deeply equal or not
---@generic T
---@param xs T[]
---@param ys T[]
---@return boolean
function M.equal(xs, ys)
  if xs == ys then
    return true
  end

  if type(xs) ~= 'table' or type(ys) ~= 'table' then
    return false
  end

  local count1 = 0
  for _ in pairs(xs) do
    count1 = count1 + 1
  end

  local count2 = 0
  for _ in pairs(ys) do
    count2 = count2 + 1
  end

  if count1 ~= count2 then
    return false
  end

  for i, v in pairs(xs) do
    if not M.equal(v, ys[i]) then
      return false
    end
  end

  return true
end

---Generates character range
---@param start_char string --The starting character of the range
---@param end_char string --The ending character of the range
---@return string[] --A list of characters from start_char to end_char
function M.char_range(start_char, end_char)
  local result = {}
  local start_code = string.byte(start_char)
  local end_code = string.byte(end_char)
  for i = start_code, end_code do
    table.insert(result, string.char(i))
  end
  return result
end

---Finds the index of a value in a list
---@generic T
---@param list T[]
---@param value T
---@return number | nil --The index of the value, or nil if not found
function M.index_of(list, value)
  for i, item in ipairs(list) do
    if item == value then
      return i
    end
  end
  return nil
end

-- TODO: Rename to contain
---Checks if a list contains a value
---@generic T
---@param list T[]
---@param value T
---@return boolean
function M.has(list, value)
  for _, item in ipairs(list) do
    if item == value then
      return true
    end
  end
  return false
end

---@generic T
---@param array T[]
---@param start_index integer
---@param end_index integer
---@return T[]
local function simple_slice(array, start_index, end_index)
  local result = {}
  local end_index = end_index or #array

  for i = start_index, end_index do
    if array[i] ~= nil then
      result[#result + 1] = array[i]
    end
  end

  return result
end

---vim.list_slice()に依存できない場合の代替
---@generic T
---@param array T[]
---@param start_index integer
---@param end_index integer
---@return T[]
function M.slice(array, start_index, end_index)
  -- 負のインデックスを正に変換
  local start_index = (start_index < 0)
    and (#array + start_index + 1)
    or start_index

  local end_index = (end_index ~= nil and end_index < 0)
    and (#array + end_index + 1)
    or end_index

  return simple_slice(array, start_index, end_index)
end

-- In-source testing
if vim == nil then
  local test = Test.test
  local assert_equal = Test.assert_equal

  test('char_range() should generates the char array correctly', function()
    assert_equal(M.char_range('a', 'z'), {
      'a',
      'b',
      'c',
      'd',
      'e',
      'f',
      'g',
      'h',
      'i',
      'j',
      'k',
      'l',
      'm',
      'n',
      'o',
      'p',
      'q',
      'r',
      's',
      't',
      'u',
      'v',
      'w',
      'x',
      'y',
      'z',
    })
  end)

  test('char_range() should handle single character range', function()
    assert_equal(M.char_range('x', 'x'), { 'x' })
  end)

  test('char_range() should handle numbers', function()
    assert_equal(M.char_range('0', '5'), { '0', '1', '2', '3', '4', '5' })
  end)

  test('equal() should compare arrays correctly', function()
    -- 同一参照
    local arr = { 1, 2, 3 }
    assert_equal(M.equal(arr, arr), true)

    -- 同じ内容
    assert_equal(M.equal({ 1, 2, 3 }, { 1, 2, 3 }), true)
    assert_equal(M.equal({ 'a', 'b' }, { 'a', 'b' }), true)
    assert_equal(M.equal({}, {}), true)

    -- 異なる内容
    assert_equal(M.equal({ 1, 2, 3 }, { 1, 2, 4 }), false)
    assert_equal(M.equal({ 1, 2 }, { 1, 2, 3 }), false)
    assert_equal(M.equal({ 1, 2, 3 }, { 1, 2 }), false)
  end)

  test('equal() should handle nested arrays', function()
    assert_equal(M.equal({ { 1, 2 }, { 3, 4 } }, { { 1, 2 }, { 3, 4 } }), true)
    assert_equal(M.equal({ { 1, 2 }, { 3, 4 } }, { { 1, 2 }, { 3, 5 } }), false)
    assert_equal(M.equal({ 1, { 2, 3 }, 4 }, { 1, { 2, 3 }, 4 }), true)
  end)

  test('index_of() should find correct index', function()
    assert_equal(M.index_of({ 1, 2, 3 }, 2), 2)
    assert_equal(M.index_of({ 'a', 'b', 'c' }, 'c'), 3)
    assert_equal(M.index_of({ 1, 2, 3 }, 1), 1)
    assert_equal(M.index_of({}, 1), nil)
    assert_equal(M.index_of({ 1, 2, 3 }, 4), nil)
  end)

  test('index_of() should return first occurrence', function()
    assert_equal(M.index_of({ 1, 2, 2, 3 }, 2), 2)
    assert_equal(M.index_of({ 'a', 'b', 'a' }, 'a'), 1)
  end)

  test('has() should check if value exists', function()
    assert_equal(M.has({ 1, 2, 3 }, 2), true)
    assert_equal(M.has({ 'a', 'b', 'c' }, 'b'), true)
    assert_equal(M.has({ 1, 2, 3 }, 4), false)
    assert_equal(M.has({}, 1), false)
  end)

  test('has() cannot find values after nil due to ipairs() limitation', function()
    assert_equal(M.has({ 1, nil, 2 }, 2), false)
  end)

  test('slice() should take a sub array from the taken array', function()
    assert_equal(M.slice({1, 2, 3, 4, 5}, 2, 4), {2, 3, 4})
  end)
end

return M
