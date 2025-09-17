-- Import all FXML snippet modules
local attr = require("snippets.fxml.attr")
local fxml = require("snippets.fxml.fxml")
local template = require("snippets.fxml.template")

-- Combine all snippets
local fxml_snippets = {}
vim.list_extend(fxml_snippets, attr)
vim.list_extend(fxml_snippets, fxml)
vim.list_extend(fxml_snippets, template)

return fxml_snippets