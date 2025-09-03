local M = {}

---Fetches webpage title from clipboard URL
---@param url string
function M.fetch_webpage_title(url)
  local cmd = string.format('curl --silent %s | pup --plain "title json{}" | jq -r ".[0].text"', vim.fn.shellescape(url))
  local handle = io.popen(cmd)
  if not handle then
    return 'Error: Failed to execute command'
  end
  local result = handle:read("*a")
  handle:close()
  return vim.trim(result)
end

return M
