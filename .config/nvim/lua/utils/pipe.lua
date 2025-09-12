---```lua
---local pipe = require('utils.pipe')
---```

---A rich polymorphic container type.
---
---@generic ValueType
---@class PipedData : { value: ValueType }
---
---Example:
---```lua
---local result = PipedData.new('hello')
---  :apply(string.upper)
---  :apply(function(s) return s .. '!' end)
---  :get()  -- 'HELLO!'
---```
---
---`pcall()` is also supported:
---```lua
---PipedData.new({pcall(get_operation)})
---  :apply_if(
---    function(result)
---      return result[1] -- result[1] is `ok: boolean`
---    end,
---    function(result)
---      local operate = result[2]
---      operate()
---    end
---  )
---```
local PipedData = {}
PipedData.__index = PipedData

---@generic ValueType
---@param value ValueType
---@return PipedData<ValueType>
function PipedData.new(value)
  local self = setmetatable({}, PipedData)
  self.value = value
  return self
end

local pipe = PipedData.new

---Returns the inside value.
---@generic ValueType
---@return PipedData<ValueType>
function PipedData:get()
  return self.value
end

---Applies a function to the inside value.
---@generic ValueType, NextType
---@param f fun(value: ValueType): NextType
---@param handle (fun(): NextType)? --If `f(self.value)` is to be an error, return `handle()` instead
---@return PipedData<NextType>
function PipedData:apply(f, handle)
  if handle == nil then
    return PipedData.new(f(self.value))
  end

  local ok, result = pcall(f, self.value)
  if not ok then
    return handle()
  end
  return PipedData.new(result)
end

---Applies `f` if `p(self.value)` is truthy.
---@generic ValueType
---@param p fun(value: ValueType): boolean
---@param f fun(value: ValueType): ValueType
---@return PipedData<ValueType>
function PipedData:apply_if(p, f)
  return p(self.value) and f(self.value) or self.value
end

-- In-source testing
if vim == nil then
  (function()
    local result = PipedData.new(42):get()
    assert(result == 42, '- Failed: get() should return the inside value simply')
  end)();

  (function()
    local result = PipedData.new('nayu'):apply(string.upper):get()
    assert(result == 'NAYU', '- Failed: apply() should apply a function to the inside value')
  end)();

  (function()
    local result = PipedData.new(42):apply(function()
      error('error')
    end, function()
      return -1
    end)
    assert(result == -1, '- Failed: apply() should handle occurred error if application failed')
  end)();

  (function()
    local ok, result = pcall(function()
      PipedData.new(42):apply(function()
        error('error')
      end)
    end)
    assert(not ok, '- Failed: apply() should be error if application failed and handle is nil')
  end)()
end

return pipe
