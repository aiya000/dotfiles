-- Import all XAML snippet modules
local namespace = require("snippets.xaml.namespace")
local xaml = require("snippets.xaml.xaml")
local tag_general = require("snippets.xaml.tag_general")
local template_tag = require("snippets.xaml.template_tag")
local template_attr = require("snippets.xaml.template_attr")
local tag = require("snippets.xaml.tag")
local xml_namespace = require("snippets.xaml.xml_namespace")

-- Combine all snippets
local xaml_snippets = {}
vim.list_extend(xaml_snippets, namespace)
vim.list_extend(xaml_snippets, xaml)
vim.list_extend(xaml_snippets, tag_general)
vim.list_extend(xaml_snippets, template_tag)
vim.list_extend(xaml_snippets, template_attr)
vim.list_extend(xaml_snippets, tag)
vim.list_extend(xaml_snippets, xml_namespace)

return xaml_snippets