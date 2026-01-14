local M = {}

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

M.is_list = M.is_array

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

---Simular to `table.insert()`, but a pure function
---@generic T
---@param xs T[] --The original list. This will not be mutated
---@param x T --The element to append
---@return T[] --A new list with the element appended
function M.append(xs, x)
  local result = { unpack(xs) }
  table.insert(result, x)
  return result
end

---Simular to `vim.list_extend()`, but can take multiple lists (varargs).
---
---@generic T
---@param ... T
---@return T[]
---
---Example:
---```lua
---concat(
---  { 1, 2, 3 },
---  { 4, 5 },
---  { 6 }
---) -- { 1, 2, 3, 4, 5, 6 }
---```
function M.concat(...)
  local result = {}
  for _, xs in ipairs({ ... }) do
    for _, x in ipairs(xs) do
      table.insert(result, x)
    end
  end
  return result
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
  end_index = end_index or #array

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
  start_index = (start_index < 0) and (#array + start_index + 1) or start_index
  end_index = (end_index ~= nil and end_index < 0) and (#array + end_index + 1) or end_index
  return simple_slice(array, start_index, end_index)
end

local symbol_of_single_to_be_replaced_by_format = tostring({})

---Gets a unique marker to be used in `format()`.
---@see format()
---@generic T --A dirty hacked type
---@return T
function M.get_marker_of_single_for_format_function()
  return symbol_of_single_to_be_replaced_by_format
end

---A shorthand for `get_marker_of_single_for_format_function()`
---NOTE: 's' is 's' of 'space'
M.s = M.get_marker_of_single_for_format_function

local symbol_of_multi_to_be_replaced_by_format = tostring({})

---Simular to `get_marker_of_single_for_format_function()`, but for multiple replacements
---@see get_marker_of_single_for_format_function()
---@see format()
---@generic T --A dirty hacked type
---@return T
function M.get_marker_of_multi_for_format_function()
  return symbol_of_multi_to_be_replaced_by_format
end

---A shorthand for `get_marker_of_multi_for_format_function()`
---NOTE: 'ss' is 'ss' of 'spaces'
M.ss = M.get_marker_of_multi_for_format_function

---@class StateOfFormat
---@field interplated_count integer
---@field result unknown[]

---Simular to `string.format()`, but for lists
---@generic T
---@param ... T
---@return T[]
---```lua
----- Embed elements
---list.format({ 1, list.s, 3 }, 2) -- { 1, 2, 3 }
---list.format({ list.s(), list.s(), list.s() }, 1, 2, 3) -- { 1, 2, 3 }
---
----- Embed lists
---list.format({ 1, list.ss(), 4 }, { 2, 3 }) -- { 1, 2, 3, 4 }
---```
function M.format(xs, ...)
  local source = { ... }

  ---@type StateOfFormat:
  local initial_state = {
    interplated_count = 0,
    result = {},
  }

  ---@param state StateOfFormat
  ---@param x unknown
  ---@return StateOfFormat
  local function append(state, x)
    return {
      interplated_count = state.interplated_count,
      result = M.append(state.result, x),
    }
  end

  ---@param state StateOfFormat
  ---@param source_ unknown[]
  ---@return StateOfFormat
  local function embed_element(state, source_)
    local filler = source_[state.interplated_count + 1] -- +1 to justify Lua's 1-based index
    return {
      interplated_count = state.interplated_count + 1,
      result = M.append(state.result, filler),
    }
  end

  ---@param state StateOfFormat
  ---@param source_ unknown[]
  ---@return StateOfFormat
  local function embed_list(state, source_)
    local filler = source_[state.interplated_count + 1] -- +1 to justify Lua's 1-based index
    return {
      interplated_count = state.interplated_count + 1,
      result = M.concat(state.result, filler),
    }
  end

  return vim.iter(xs)
    :fold(initial_state, function(state, x)
      if x == M.s() then
        return embed_element(state, source)
      end

      if x == M.ss() then
        return embed_list(state, source)
      end

      return append(state, x)
    end)
    .result
end



return M
