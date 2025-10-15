vim.keymap.set('n', 'p', 'pi', { buffer = true }) -- Enter insert mode after pasting

---OSC 51 handler function for opening files from terminal
---This is called via escape sequences from shell commands
_G.Tapi_OpenFileInParent = function(filepath)
  if not filepath or filepath == '' then
    vim.notify('Tapi_OpenFileInParent: No filepath provided', vim.log.levels.ERROR)
    return
  end

  -- Convert relative path to absolute if needed
  if not string.match(filepath, '^/') then
    local cwd = vim.fn.getcwd()
    filepath = cwd .. '/' .. filepath
  end

  vim.schedule(function()
    vim.cmd('edit ' .. vim.fn.fnameescape(filepath))
  end)
end

-- Setup shell commands using OSC 51 escape sequences
local function setup_shell_commands()
  if vim.bo.buftype ~= 'terminal' then
    return
  end

  local job_id = vim.b.terminal_job_id
  if not job_id then
    return
  end

  -- Create shell function script that uses OSC 51 escape sequences
  local nvim_cache = vim.fn.stdpath('cache')
  local script_path = nvim_cache .. '/nvim_parent_open_' .. job_id .. '.sh'

  -- OSC 51 format: \e]51;["call", "function_name", ["arg1", "arg2", ...]]\a
  -- We'll use a simpler approach with printf and escape sequences
  local script_content = [=[
    #!/bin/bash
    # Neovim parent file opening via OSC 51 escape sequences
    # Auto-generated for terminal integration

    function nvim_parent_edit() {
      if [ $# -eq 0 ]; then
        echo "Usage: nvim <filename>"
        return 1
      fi

      # Convert relative path to absolute
      local filepath="$1"
      if [[ "$filepath" != /* ]]; then
        filepath="$(reaplath "$filepath")"
      fi
      printf '\033]51;["call","Tapi_OpenFileInParent",["%s"]]\007' "$filepath"
    }

    # Aliases for convenience
    alias nvim='nvim_parent_edit'
    echo '✓ Parent Neovim integration ready. Use: `nvim <filename>`'
  ]=]

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

  -- Send command to source the script in the terminal
  vim.defer_fn(function()
    local source_cmd = 'source ' .. script_path .. '\n'
    vim.fn.chansend(job_id, source_cmd)
  end, 2000)

  -- Cleanup on buffer delete
  vim.api.nvim_create_autocmd('BufDelete', {
    buffer = 0,
    once = true,
    callback = function()
      vim.fn.delete(script_path)
    end,
  })
end

vim.defer_fn(setup_shell_commands, 400)
