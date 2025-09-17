-- Import all Hamlet snippet modules
local hamlet = require("snippets.hamlet.hamlet")
local attr = require("snippets.hamlet.attr")

-- Combine all snippets
local hamlet_snippets = {}
vim.list_extend(hamlet_snippets, hamlet)
vim.list_extend(hamlet_snippets, attr)

return hamlet_snippets