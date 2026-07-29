---commands.lua のサブセット。lightweight版から使われるコマンドのみ定義する。

local list = require('utils.list')
local nvim = require('nvim-lightweight')

local function create_command(cmd_name, func, options)
  options = vim.tbl_extend('keep', options or {}, { bar = true })
  vim.api.nvim_create_user_command(cmd_name, func, options)
end

-- text utils {{{

create_command('Grep', function(opts)
  require('telescope.builtin').grep_string({ search = opts.args })
end, { nargs = 1 })

create_command('RemoveTrailingSpacesWithForce', function(opts)
  local range = opts.range > 0 and { opts.line1, opts.line2 } or nil
  nvim.remove_trailing_spaces(true, range)
end, { range = true })

create_command('ReverseLines', function(opts)
  local bufnr = vim.api.nvim_get_current_buf()
  local lines = vim.api.nvim_buf_get_lines(bufnr, opts.line1 - 1, opts.line2, false)
  vim.api.nvim_buf_set_lines(bufnr, opts.line1 - 1, opts.line2, false, list.reverse(lines))
end, {
  range = true,
  desc = 'Reverse the order of lines in the selected range or entire buffer',
})

-- }}}
-- Others {{{

create_command('CClear', function()
  vim.fn.setqflist({})
end, { desc = 'Clear quickfix' })

-- }}}

-- vim: foldmethod=marker
