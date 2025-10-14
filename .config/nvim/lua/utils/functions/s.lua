local M = {}

---@see s
---For Lua 5.1 compatibility
---@param ref { code: string, context: table }
---@return string --The interplated string
local function replace_expr_in_context_for_lua_5_1_non_jit(ref)
  local f = loadstring(ref.code)
  if f == nil then
    error('Failed to compile expression: ' .. ref.expr)
  end
  setfenv(f, ref.context)

  local ok, result = pcall(f)
  if not ok then
    error('Error evaluating expression: ' .. ref.expr .. ' - ' .. tostring(result))
  end

  return tostring(result)
end

---This function is same as `replace_expr_in_context_for_lua_5_1_non_jit()`,
---but for LuaJIT/Lua 5.2+ (Neovim).
---@see s
---@see replace_expr_in_context_for_lua_5_1_non_jit
local function replace_expr_in_context(ref)
  local f = load(ref.code, nil, nil, ref.context)
  if f == nil then
    error('Failed to compile expression: ' .. ref.expr)
  end

  local ok, result = pcall(f)
  if not ok then
    error('Error evaluating expression: ' .. ref.expr .. ' - ' .. tostring(result))
  end

  return tostring(result)
end

---`('%s'):format(text)`を使うと、各項の役割がわかりにくい時用の関数
---@param text string --テンプレートリテラルっぽい文字列
---@param vars table --変数テーブル
---@return string --変数埋め込み済み文字列
---```lua
----- 変数の埋め込み
---local msg = s('Hello {name}! Next year you will be {age + 1}.', {
---  name = 'Alice',
---  age = 19,
---})
---
----- 複数回の変数の使用
---local x = s('Hello, {foo}! and {foo}!', { foo = 'world' })
---
----- 式の評価
---s('{x + y}', { x = 5, y = 3 })      -- '8'
---s('{math.pi}', {})                  -- '3.1415926535898' (グローバル変数)
---s('{name or "Unknown"}', { name = nil }) -- 'Unknown'
---```
function M.s(text, vars)
  if type(vars) ~= 'table' then
    error('s() requires a variable table as the second argument')
  end

  setmetatable(vars, {
    __index = function(_, key)
      local global_value = _G[key]
      if global_value ~= nil then
        return global_value
      end
      error('undefined variable: ' .. tostring(key))
    end,
  })

  return text:gsub('{([^}]+)}', function(expr)
    local replacing_ref = {
      code = 'return ' .. expr,
      context = vars,
      expr = expr,
    }
    return (
      (_VERSION == 'Lua 5.1' and not jit) and replace_expr_in_context_for_lua_5_1_non_jit
      or replace_expr_in_context
    )(replacing_ref)
  end)
end

-- In-source testing

return M
