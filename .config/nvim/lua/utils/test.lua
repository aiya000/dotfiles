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
function M.assert_equal(actual, expected)
  if actual ~= expected then
    error(string.format('Expected: %s, but got: %s', tostring(expected), tostring(actual)))
  end
end

return M
