---```lua
---local pipe = require('utils.pipe')
---```

---A rich polymorphic container type
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

---Returns the inside value
---@generic ValueType
---@return PipedData<ValueType>
function PipedData:get()
  return self.value
end

---Applies a function to the inside value
---@generic ValueType, NextType
---@param f fun(value: ValueType): NextType
---@param handle? fun(): NextType --If `f(self.value)` is to be an error, return `handle()` instead
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

---Applies `f` if `p(self.value)` is truthy
---@generic ValueType
---@param p fun(value: ValueType): boolean
---@param f fun(value: ValueType): ValueType
---@param handle? fun(): ValueType --If `p(self.value)` is truthy but `f(self.value)` is to be an error, return `handle()` instead. Note that this is not called when `not p(self.value)`
---@return PipedData<ValueType>
function PipedData:apply_if(p, f, handle)
  return p(self.value) and self:apply(f, handle) or self
end

---Applies `if_true` if `p(self.value)` is truthy, otherwise applies `if_false`
---@generic ValueType, TrueResult, FalseResult
---@param p fun(value: ValueType): boolean
---@param if_true fun(value: ValueType): TrueResult
---@param if_false fun(value: ValueType): FalseResult
---@param handle? fun(): TrueResult | FalseResult
---@return PipedData<TrueResult | FalseResult>
function PipedData:apply_conditional(p, if_true, if_false, handle)
  return p(self.value) and self:apply(if_true, handle) or self:apply(if_false, handle)
end

---Applies `f` if `self.value ~= nil`
---@generic ValueType, NextType
---@param f fun(value: ValueType): NextType
---@param handle? fun(): NextType
---@return PipedData<NextType>
function PipedData:apply_if_not_nil(f, handle)
  return self:apply_if(function(value)
    return value ~= nil
  end, f, handle)
end

---Calls a `self.value`'s `method` method, and returns the result.
---This means calling of `self.value[method](self.value, ...)`.
---@generic ValueType, NextType
---@param method string
---@param ... unknown
---@return PipedData<NextType>
function PipedData:apply_method(method, ...)
  local args = { ... }
  return self:apply(function()
    return self.value[method](self.value, unpack(args))
  end)
end

---Simular to `apply_if_not_nil()` and `apply_method()`
---@generic ValueType, NextType
---@param method_name string
---@param handle? fun(): NextType
---@return PipedData<NextType>
function PipedData:apply_method_if_not_nil(method_name, handle)
  return self:apply_if_not_nil(function(value)
    return value[method_name](value)
  end, handle)
end

-- In-source testing

return pipe
