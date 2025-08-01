---@class ToggleBuftype
local ToggleBuftype = {}
ToggleBuftype.__index = ToggleBuftype

---@return ToggleBuftype
function ToggleBuftype.new()
  local self = setmetatable({}, ToggleBuftype)
  self.backedup_buftype = ''
  return self
end

function ToggleBuftype:backup_buftype()
  if vim.bo.buftype == 'nofile' then
    self.backedup_buftype = 'nofile'
    vim.bo.buftype = ''
  end
end

function ToggleBuftype:restore_buftype()
  if self.backedup_buftype == 'nofile' then
    self.backedup_buftype = ''
    vim.bo.buftype = 'nofile'
  end
end

return ToggleBuftype
