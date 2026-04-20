-- NOTE: When you add LuaRocks packages, you need to run `:LuarocksRefreshCache` to update the cache of LuaRocks paths

-- Ensure LuaRocks is installed and available in PATH
if vim.fn.executable('luarocks') ~= 1 then
  error('LuaRocks is not found. Please make sure it is in your PATH.')
end

local cache_path = vim.fn.stdpath('cache') .. '/luarocks-paths.lua'

local function trim_trailing_separators(s)
  return s:gsub('[;%s]+$', '')
end

local function parse_luarocks_output(output)
  local lua_path = output:match('LUA_PATH%s*=%s*"([^"]+)"')
    or output:match("LUA_PATH%s*=%s*'([^']+)'")
    or output:match('LUA_PATH%s*=%s*([^\n]+)')
  local lua_cpath = output:match('LUA_CPATH%s*=%s*"([^"]+)"')
    or output:match("LUA_CPATH%s*=%s*'([^']+)'")
    or output:match('LUA_CPATH%s*=%s*([^\n]+)')
  if lua_path then
    lua_path = trim_trailing_separators(lua_path)
  end
  if lua_cpath then
    lua_cpath = trim_trailing_separators(lua_cpath)
  end
  return lua_path, lua_cpath
end

local cached_lua_path = nil
local cached_lua_cpath = nil

local function apply_paths(lua_path, lua_cpath)
  if lua_path and lua_path ~= '' then
    package.path = package.path .. ';' .. lua_path
  end
  if lua_cpath and lua_cpath ~= '' then
    package.cpath = package.cpath .. ';' .. lua_cpath
  end
end

local function save_cache(lua_path, lua_cpath)
  local f = io.open(cache_path, 'w')
  if not f then
    return
  end

  f:write('return ' .. vim.inspect({ lua_path = lua_path, lua_cpath = lua_cpath }) .. '\n')
  f:close()
end

local function load_from_cache()
  local f = io.open(cache_path, 'r')
  if not f then
    return false
  end

  local chunk = f:read('*a')
  f:close()
  local fn = load(chunk)
  if not fn then
    return false
  end

  local data = fn()
  if type(data) ~= 'table' then
    return false
  end

  cached_lua_path = data.lua_path
  cached_lua_cpath = data.lua_cpath
  return true
end

local function build_and_save_cache()
  local handle, popen_err = io.popen('luarocks path --lua-version 5.1')
  if not handle then
    error('Failed to run "luarocks path": ' .. (popen_err or 'unknown error'))
  end
  local result = handle:read('*a') or ''
  handle:close()
  cached_lua_path, cached_lua_cpath = parse_luarocks_output(result)
  save_cache(cached_lua_path, cached_lua_cpath)
end

function refresh_cache()
  build_and_save_cache()
  apply_paths(cached_lua_path, cached_lua_cpath)
end

-- Add LuaRocks paths to Neovim's package.path and package.cpath.
-- Uses cache to avoid spawning `luarocks path` on every startup.
-- Run :LuarocksRefreshCache after installing/removing luarocks packages.
local function add_luarocks_paths()
  if cached_lua_path == nil and not load_from_cache() then
    build_and_save_cache()
  end
  apply_paths(cached_lua_path, cached_lua_cpath)
end

add_luarocks_paths()

return {
  add_luarocks_paths = add_luarocks_paths,
  refresh_cache = refresh_cache,
}
