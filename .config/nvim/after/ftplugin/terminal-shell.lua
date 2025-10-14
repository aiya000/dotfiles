vim.keymap.set('n', 'p', 'pi', { buffer = true }) -- Enter insert mode after pasting

-- ParentEdit command for opening files in parent Neovim
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
  desc = 'Open file in parent Neovim',
})

-- Setup shell command approach for opening files in parent Neovim
local function setup_shell_commands()
  if vim.bo.buftype ~= 'terminal' then
    return
  end

  local job_id = vim.b.terminal_job_id
  if not job_id then
    return
  end

  -- Create a communication file for this terminal session
  local request_file = vim.fn.stdpath('cache') .. '/nvim_parent_request_' .. job_id

  -- Create a shell function script
  local nvim_cache = vim.fn.stdpath('cache')
  local script_path = nvim_cache .. '/nvim_parent_open_' .. job_id .. '.sh'

  -- Create the shell function script
  local script_content = string.format([[#!/bin/bash
# Neovim parent file opening functions
# Auto-generated for terminal integration

nvim_parent_edit() {
  if [ $# -eq 0 ]; then
    echo "Usage: e <filename> or nvim <filename>"
    return 1
  fi
  
  local filepath="$1"
  # Convert relative path to absolute
  if [[ "$filepath" != /* ]]; then
    filepath="$(pwd)/$filepath"
  fi
  
  # Write filepath to request file for parent Neovim to monitor
  echo "$filepath" > "%s"
  echo "Opening in parent Neovim: $filepath"
}

# Aliases for convenience
alias e='nvim_parent_edit'
alias nvim='nvim_parent_edit'
]], request_file)

  -- Write the script file
  local file = io.open(script_path, 'w')
  if file then
    file:write(script_content)
    file:close()
    vim.fn.system('chmod +x ' .. script_path)
  else
    vim.notify('Failed to create parent edit script', vim.log.levels.ERROR)
    return
  end

  -- Setup file watcher for the request file
  local timer = vim.loop.new_timer()
  if not timer then
    vim.notify('Failed to create timer for file monitoring', vim.log.levels.ERROR)
    return
  end

  -- Monitor for file edit requests
  timer:start(
    100, -- Start after 100ms
    200, -- Check every 200ms
    vim.schedule_wrap(function()
      if vim.fn.filereadable(request_file) == 1 then
        local content = vim.fn.readfile(request_file)
        if #content > 0 and content[1] ~= '' then
          local filepath = content[1]
          -- Delete the request file immediately
          vim.fn.delete(request_file)
          -- Open the file in parent Neovim
          vim.schedule(function()
            vim.cmd('edit ' .. vim.fn.fnameescape(filepath))
          end)
        end
      end
    end)
  )

  -- Cleanup on buffer delete
  vim.api.nvim_create_autocmd('BufDelete', {
    buffer = 0,
    once = true,
    callback = function()
      if timer then
        timer:stop()
        timer:close()
      end
      vim.fn.delete(request_file)
      vim.fn.delete(script_path)
    end,
  })

  -- Send command to source the script in the terminal
  vim.defer_fn(function()
    local source_cmd = 'source ' .. script_path .. '\n'
    vim.fn.chansend(job_id, source_cmd)
    
    -- Display a confirmation message
    vim.defer_fn(function()
      local msg = 'echo "✓ Parent Neovim integration ready. Use: e <filename>"\n'
      vim.fn.chansend(job_id, msg)
    end, 100)
  end, 250)
end

-- Setup when terminal buffer opens
vim.api.nvim_create_autocmd('TermOpen', {
  buffer = 0,
  callback = function()
    vim.defer_fn(setup_shell_commands, 400)
  end,
})

-- Setup if already in terminal
if vim.bo.buftype == 'terminal' then
  vim.defer_fn(setup_shell_commands, 400)
end
