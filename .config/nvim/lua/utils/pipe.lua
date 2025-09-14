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
if vim == nil then
  local Test = require('utils.test')
  local test = Test.test
  local assert_equal = Test.assert_equal

  test('get() should return the inside value simply', function()
    local result = pipe(10):get()
    assert_equal(result, 10)
  end)

  test('apply() should apply a function to the inside value', function()
    local result = pipe('nayu'):apply(string.upper):get()
    assert_equal(result, 'NAYU')
  end)

  test('apply() should handle occurred error if application failed', function()
    local result = pipe(10):apply(function()
      error('error')
    end, function()
      return -1
    end)
    assert_equal(result, -1)
  end)

  test('apply() should be error if application failed and handle is nil', function()
    local ok, _ = pcall(function()
      pipe(10):apply(function()
        error('error')
      end)
    end)
    assert_equal(ok, false)
  end)

  test('apply_if() should apply function when predicate is true', function()
    local result = pipe(10)
      :apply_if(function(x)
        return x > 5
      end, function(x)
        return x * 2
      end)
      :get()
    assert_equal(result, 20)
  end)

  test('apply_if() should not apply function when predicate is false', function()
    local result = pipe(3)
      :apply_if(function(x)
        return x > 5
      end, function(x)
        return x * 2
      end)
      :get()
    assert_equal(result, 3)
  end)

  test('apply_if() should handle error with handle function', function()
    local result = pipe(10):apply_if(function(x)
      return x > 5
    end, function()
      error('error')
    end, function()
      return -1
    end)
    assert_equal(result, -1)
  end)

  test('apply_conditional() should apply if_true when predicate is true', function()
    local result = pipe(10)
      :apply_conditional(function(x)
        return x > 5
      end, function(x)
        return x * 2
      end, function(x)
        return x + 100
      end)
      :get()
    assert_equal(result, 20)
  end)

  test('apply_conditional() should apply if_false when predicate is false', function()
    local result = pipe(3)
      :apply_conditional(function(x)
        return x > 5
      end, function(x)
        return x * 2
      end, function(x)
        return x + 100
      end)
      :get()
    assert_equal(result, 103)
  end)

  test('apply_if_not_nil() should apply function when value is not nil', function()
    local result = pipe('nayu'):apply_if_not_nil(string.upper):get()
    assert_equal(result, 'NAYU')
  end)

  test('apply_if_not_nil() should not apply function when value is nil', function()
    local result = pipe('nayu')
      :apply(function()
        return nil
      end)
      :apply_if_not_nil(string.upper)
      :get()
    assert_equal(result, nil)
  end)

  test('apply_if_not_nil() should handle error with handle function', function()
    local result = pipe('nayu'):apply_if_not_nil(function()
      error('error')
    end, function()
      return 'handled'
    end)
    assert_equal(result, 'handled')
  end)

  test('apply_method() should call method of self.value', function()
    local result = pipe({
        number = 10,
        get_number = function(self)
          return self.number
        end,
      })
      :apply_method('get_number')
      :get()
    assert_equal(result, 10)
  end)

  test('apply_method_if_not_nil() should call method when value is not nil', function()
    local result = pipe({
        number = 10,
        get_double = function(self)
          return self.number * 2
        end,
      })
      :apply_method_if_not_nil('get_double')
      :get()
    assert_equal(result, 20)
  end)

  test('apply_method_if_not_nil() should not apply function when value is nil', function()
    local result = pipe({
        number = 10,
        get_double = function(self)
          return self.number * 2
        end,
      })
      :apply(function()
        return nil
      end)
      :apply_method_if_not_nil('get_double')
      :get()
    assert_equal(result, nil)
  end)
end

return pipe
