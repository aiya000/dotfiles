-- Ensure LuaRocks is installed and available in PATH
if vim.fn.executable('luarocks') ~= 1 then
  error('LuaRocks is not found. Please make sure it is in your PATH.')
end

-- Add LuaRocks paths to Neovim's package.path and package.cpath
-- Note: Explicitly request Lua 5.1 (Neovim's LuaJIT version) paths from LuaRocks
local function add_luarocks_paths()
  local handle, popen_err = io.popen('luarocks path --lua-version 5.1')
  if not handle then
    error('Failed to run "luarocks path": ' .. (popen_err or 'unknown error'))
  end

  local result = handle:read('*a') or ''
  handle:close()

  local function trim_trailing_separators(s)
    return (s:gsub('[;%s]+$', ''))
  end

  -- Extract LUA_PATH from the shell commands printed by `luarocks path`
  local lua_path = result:match('LUA_PATH%s*=%s*"([^"]+)"')
                or result:match("LUA_PATH%s*=%s*'([^']+)'")
                or result:match('LUA_PATH%s*=%s*([^\n]+)')
  if lua_path and lua_path ~= '' then
    lua_path = trim_trailing_separators(lua_path)
    if lua_path ~= '' then
      package.path = package.path .. ';' .. lua_path
    end
  end

  -- Extract LUA_CPATH from the shell commands printed by `luarocks path`
  local lua_cpath = result:match('LUA_CPATH%s*=%s*"([^"]+)"')
                 or result:match("LUA_CPATH%s*=%s*'([^']+)'")
                 or result:match('LUA_CPATH%s*=%s*([^\n]+)')
  if lua_cpath and lua_cpath ~= '' then
    lua_cpath = trim_trailing_separators(lua_cpath)
    if lua_cpath ~= '' then
      package.cpath = package.cpath .. ';' .. lua_cpath
    end
  end
end

add_luarocks_paths()
