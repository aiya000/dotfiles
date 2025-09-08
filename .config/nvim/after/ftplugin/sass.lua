local helper = require('helper')

vim.opt_local.tabstop = 2
vim.opt_local.shiftwidth = 2
vim.opt_local.expandtab = true

local function arrange_html_attribute_to_sass_attribute()
  local start_line = vim.fn.line("'<")
  local end_line = vim.fn.line("'>")
  
  for line_num = start_line, end_line do
    vim.fn.cursor(line_num, 1)
    -- Replace ="..." with : ...
    vim.cmd('s/="/: /')
    -- Remove trailing "
    vim.cmd('s/"$//')
    -- Go to beginning of word
    vim.cmd('normal! _')
    -- Convert camelCase to snake_case using operator-decamelize plugin
    M.run_with_virtual_keymaps('viw<Plug>(operator-decamelize)')
    -- Replace _ with -
    vim.cmd('s/_/-/g')
  end
end

vim.keymap.set('v', 'i{', arrange_html_attribute_to_sass_attribute, { buffer = true, silent = true })
