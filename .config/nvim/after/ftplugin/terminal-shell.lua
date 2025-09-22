vim.keymap.set('n', 'p', 'pi', { buffer = true }) -- Enter insert mode after pasting

-- COMMENTED OUT: <C-f> keymap (causing issues)
-- -- Keep the simple keymap approaches too
-- vim.keymap.set('t', '<C-f>', function()
--   local input = vim.fn.input('Open file in parent: ')
--   if input and input ~= '' then
--     local filepath = input
--     if not string.match(filepath, '^/') then
--       local cwd = vim.fn.getcwd()
--       filepath = cwd .. '/' .. filepath
--     end
--     vim.cmd('stopinsert')
--     vim.schedule(function()
--       vim.cmd('edit ' .. vim.fn.fnameescape(filepath))
--     end)
--   end
-- end, { buffer = true, desc = 'Open file in parent Neovim' })

vim.api.nvim_buf_create_user_command(0, 'ParentEdit', function(opts)
  local filepath = opts.args
  if filepath and filepath ~= '' then
    if not string.match(filepath, '^/') then
      local cwd = vim.fn.getcwd()
      filepath = cwd .. '/' .. filepath
    end
    vim.schedule(function()
      vim.cmd('edit ' .. vim.fn.fnameescape(filepath))
    end)
  else
    print('Usage: :ParentEdit <filename>')
  end
end, {
  nargs = 1,
  complete = 'file',
  desc = 'Open file in parent Neovim'
})

-- COMMENTED OUT: Shell command approach implementation (causing issues)
-- -- Now setup shell command approach with better implementation
-- local function setup_shell_commands()
--   if vim.bo.buftype ~= 'terminal' then
--     return
--   end
--
--   local job_id = vim.b.terminal_job_id
--   if not job_id then
--     return
--   end
--
--   -- Create a simple script for opening files
--   local nvim_cache = vim.fn.stdpath('cache')
--   local script_path = nvim_cache .. '/parent_open_' .. job_id .. '.sh'
--
--   -- Create the script content
--   local script_lines = {
--     '#!/bin/bash',
--     '# Auto-generated script for opening files in parent Neovim',
--     '',
--     'if [ $# -eq 0 ]; then',
--     '    echo "Usage: $0 <filename>"',
--     '    exit 1',
--     'fi',
--     '',
--     'FILEPATH="$1"',
--     'if [[ "$FILEPATH" != /* ]]; then',
--     '    FILEPATH="$(pwd)/$FILEPATH"',
--     'fi',
--     '',
--     'REQUEST_FILE="/tmp/nvim_parent_' .. job_id .. '"',
--     'echo "$FILEPATH" > "$REQUEST_FILE"',
--     'echo "Requested to open: $FILEPATH"'
--   }
--
--   -- Write the script
--   local file = io.open(script_path, 'w')
--   if file then
--     for _, line in ipairs(script_lines) do
--       file:write(line .. '\n')
--     end
--     file:close()
--     vim.fn.system('chmod +x ' .. script_path)
--   end
--
--   -- Send shell setup commands
--   local commands = {
--     '# Setup parent Neovim file opening',
--     'alias e="' .. script_path .. '"',
--     'alias nvim="' .. script_path .. '"',
--     'echo "Parent Neovim file opening setup complete! Use: e filename"',
--     ''
--   }
--
--   -- Send commands with delay
--   vim.defer_fn(function()
--     for _, cmd in ipairs(commands) do
--       vim.fn.chansend(job_id, cmd .. '\r')
--       vim.defer_fn(function() end, 50)  -- Small delay between commands
--     end
--   end, 200)
--
--   -- Monitor for requests
--   local request_file = '/tmp/nvim_parent_' .. job_id
--   local timer = vim.loop.new_timer()
--
--   timer:start(300, 300, vim.schedule_wrap(function()
--     if vim.fn.filereadable(request_file) == 1 then
--       local content = vim.fn.readfile(request_file)
--       if #content > 0 and content[1] ~= '' then
--         local filepath = content[1]
--         vim.schedule(function()
--           vim.cmd('edit ' .. vim.fn.fnameescape(filepath))
--         end)
--         vim.fn.delete(request_file)
--       end
--     end
--   end))
--
--   -- Cleanup on buffer delete
--   vim.api.nvim_create_autocmd('BufDelete', {
--     buffer = 0,
--     callback = function()
--       if timer then
--         timer:stop()
--         timer:close()
--       end
--       vim.fn.delete(request_file)
--       vim.fn.delete(script_path)
--     end
--   })
-- end
--
-- -- Setup when terminal opens
-- vim.api.nvim_create_autocmd('TermOpen', {
--   buffer = 0,
--   callback = function()
--     vim.defer_fn(setup_shell_commands, 300)
--   end
-- })
--
-- -- Setup if already in terminal
-- if vim.bo.buftype == 'terminal' then
--   vim.defer_fn(setup_shell_commands, 300)
-- end
