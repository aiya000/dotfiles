-- XAML syntax
vim.cmd('runtime syntax/xml.vim')

vim.cmd([[
  highlight xamlNotice term=bold ctermfg=235 ctermbg=108 guifg=#262626 guibg=#87af87
]])

-- Set up match highlighting using Lua
vim.fn.matchadd('xamlNotice', 'x:Name')
vim.fn.matchadd('xamlNotice', 'x:Key')
-- Check explicit default value
vim.fn.matchadd('xamlNotice', '\\<0,0,0,0\\>')
vim.fn.matchadd('xamlNotice', 'VerticalAlignment="Stretch"')
vim.fn.matchadd('xamlNotice', 'HorizontalAlignment="Stretch"')
vim.fn.matchadd('xamlNotice', 'Text=""')
vim.fn.matchadd('xamlNotice', 'Content=""')
vim.fn.matchadd('xamlNotice', 'Margin="0"')
vim.fn.matchadd('xamlNotice', 'Padding="0"')