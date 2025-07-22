local M = {}

-- TODO: Recursively
function M.print_table(t)
  print('{')
  for k, v in pairs(t) do
    print('  ' .. k, '=', v .. ',')
  end
  print('}')
end

---Example:
---```lua
---local result = pipe('hello')
---  :map(string.upper)
---  :map(function(s) return s .. '!' end)
---  :get()  -- 'HELLO!'
---```
function M.pipe(value)
  return {
    value = value,
    let = function(self, f)
      return M.pipe(f(self.value))
    end,
    get = function(self)
      return self.value
    end,
  }
end

---Example:
---```lua
---local f = compose(
---  string.upper,
---  function(s) return s .. '!' end
---)
---f('hello')  -- 'HELLO!'
---```
function M.compose(...)
  local fs = { ... }
  return function(value)
    for i = 1, #fs do
      value = fs[i](value)
    end
    return value
  end
end

---Example:
---```lua
---local name = 'Alice'
---local age = 30
---local msg = s('Hello {name}! Next year you'll be {age + 1}.', {
---  name = name,
---  age = age,
---})
---```
function M.s(text, env)
  return text:gsub('{([^}]+)}', function(expr)
    local f = load('return' .. expr, nil, nil, env)
    return f and f() or '{' .. expr .. '}'
  end)
end

return M
