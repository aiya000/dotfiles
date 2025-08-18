local M = {}

---Meaning of a reference to a value.
---To enhance performance.
---Makes it explicit that the table has only one value.
---
---@generic ValueType
---@param value ValueType
---@return { value: ValueType } --a
---
---Example:
---```lua
---local num = ref(10)
---print(num.value) -- 10
---```
function M.ref(value)
  return { value = value }
end

-- In-source testing
if vim == nil then
  local Test = require('utils.test')
  local test = Test.test
  local assert_equal = Test.assert_equal

  test('ref() should create a table with a value', function()
    local num = M.ref(10)
    assert_equal(num.value, 10)
  end)
end

return M.ref
