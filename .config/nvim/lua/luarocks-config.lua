---Enables packages installed by `luarocks install --local`

if vim.fn.executable('luarocks') ~= 1 then
  error('Luarocks is not found. Please make sure it is in your PATH.')
end

local handle = io.popen('luarocks path --lr-path')
if handle then
  local luarocks_path = handle:read('*a'):gsub('\n', '')
  handle:close()
  if luarocks_path ~= '' then
    package.path = package.path .. ';' .. luarocks_path
  end
end

handle = io.popen('luarocks path --lr-cpath')
if handle then
  local luarocks_cpath = handle:read('*a'):gsub('\n', '')
  handle:close()
  if luarocks_cpath ~= '' then
    package.cpath = package.cpath .. ';' .. luarocks_cpath
  end
end

local function validate(package)
  local ok = pcall(require, package)
  if not ok then
    error('Luarocks package not found: ' .. package)
  end
end

validate('chotto')
validate('luarrow')
