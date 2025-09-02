local M = {}

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

return M
