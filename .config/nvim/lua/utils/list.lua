-- List utilities (replacement for vital#vimrc#import('Data.List'))

local M = {}

-- Check if list contains value
function M.has(list, value)
  for _, item in ipairs(list) do
    if item == value then
      return true
    end
  end
  return false
end

-- Find first item matching predicate, return default if not found
function M.find(list, default, predicate)
  for _, item in ipairs(list) do
    if predicate(item) then
      return item
    end
  end
  return default
end

-- Filter list items that match predicate
function M.filter(list, predicate)
  local result = {}
  for _, item in ipairs(list) do
    if predicate(item) then
      table.insert(result, item)
    end
  end
  return result
end

-- Map function over list items
function M.map(list, func)
  local result = {}
  for i, item in ipairs(list) do
    result[i] = func(item)
  end
  return result
end

-- Map function over list items with index
function M.map_with_index(list, func)
  local result = {}
  for i, item in ipairs(list) do
    result[i] = func(i - 1, item) -- 0-indexed to match Vim behavior
  end
  return result
end

-- Generate character range (like vim's range())
function M.char_range(start_char, end_char)
  local result = {}
  local start_code = string.byte(start_char)
  local end_code = string.byte(end_char)

  for code = start_code, end_code do
    table.insert(result, string.char(code))
  end

  return result
end

-- Flatten nested lists
function M.flatten(list)
  local result = {}
  for _, item in ipairs(list) do
    if type(item) == 'table' then
      vim.list_extend(result, M.flatten(item))
    else
      table.insert(result, item)
    end
  end
  return result
end

-- Get unique items from list
function M.uniq(list)
  local seen = {}
  local result = {}

  for _, item in ipairs(list) do
    if not seen[item] then
      seen[item] = true
      table.insert(result, item)
    end
  end

  return result
end

return M
