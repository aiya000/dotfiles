local helper = require('helper')
local s = require('utils.functions').s

vim.opt_local.cursorline = true

---Runs `git stash push --message "{message}" -- "{file_to_save}"`
---@param file_to_save string ---The path of a file to stash
local function stash_push(file_to_save)
  local message = vim.fn.input('Stash message: ')
  if message == '' then
    return
  end

  local cmd = s('git stash push --message "{message}" -- "{file_to_save}"', { message = message, file_to_save = file_to_save })

  vim.system({'git', 'stash', 'push', '--message', message, '--', file_to_save}, {
    text = true,
  }, function(result)
    vim.schedule(function()
      if result.code == 0 then
        vim.notify(result.stdout or 'Stash created successfully', vim.log.levels.INFO)
        vim.cmd('GinStatus') -- Refresh
      else
        vim.notify(s('Stash failed: {error}', { error = result.stderr or 'Unknown error' }), vim.log.levels.ERROR)
        vim.cmd('GinStatus') -- Refresh
      end
    end)
  end)
end

---Runs git-stash with the file on the current line
local function run_stash_push_message()
  local filename = vim.fn.trim(vim.fn.getline('.'))
  if filename ~= nil and filename ~= '' then
    stash_push(filename)
    return
  end
  error('No filename found on the current line')
end

local function run_add_patch()
  local line = vim.fn.getline('.')
  local filename = line:match('%s*(.-)%s*$') -- Trim whitespace
  if filename and filename ~= '' then
    vim.fn.termopen({'git', 'add', '--patch', filename}, {
      on_exit = function()
        vim.cmd('close')
      end
    })
  end
end

---@param subcmd_list string[] --Arguments after `:Gin commit --verbose`
local function open_commit_buffer(subcmd_list)
  subcmd_list = subcmd_list or {}

  local ok = pcall(function()
    local cmd_list = {'Gin', 'commit', '--verbose'}
    for _, subcmd in ipairs(subcmd_list) do
      table.insert(cmd_list, subcmd)
    end
    vim.cmd(table.concat(cmd_list, ' '))
  end)
end

local function force_show_stash_size()
  vim.system({'git', 'stash', 'list'}, {
    text = true,
  }, function(result)
    vim.schedule(function()
      if result.code == 0 then
        local lines = vim.split(result.stdout or '', '\n')
        local size = #lines

        if lines[1] == '' then
          size = 0
        end

        if size > 0 then
          local topline = vim.fn.getline(1)
          local new_topline = s('{topline} [stash:{size}]', { topline = topline, size = size })

          vim.bo.modifiable = true
          vim.fn.setline(1, new_topline)
          vim.bo.modifiable = false
        end
      end
    end)
  end)
end

vim.keymap.set('n', 'Q', function()
  vim.cmd('bdelete!')
end, { buffer = true, silent = true })

vim.keymap.set('n', 'A', run_add_patch, { buffer = true, silent = true })
vim.keymap.set('n', 'o', ':<C-u>vsp<CR><Plug>(gin-action-edit)', { buffer = true, silent = true })

vim.keymap.set('n', 'O', function()
  vim.cmd('normal "zyy')
  vim.cmd('tabnew')
  vim.cmd('edit ' .. vim.fn.trim(vim.fn.getreg('z')))
end, { buffer = true, silent = true })

vim.keymap.set('n', '<C-r>', '<Cmd>GinStatus<CR>', { buffer = true, silent = true })
vim.keymap.set('n', 'p', '<Plug>(gin-action-diff:smart:vsplit)', { buffer = true, silent = true, nowait = true })
vim.keymap.set('n', 'sa', '<Plug>(gin-action-stash)', { buffer = true, silent = true })
vim.keymap.set('n', 'S', run_stash_push_message, { buffer = true, silent = true })
vim.keymap.set('n', 'sp', 'Gin stash pop', { buffer = true, silent = true })
vim.keymap.set('n', 'cc', open_commit_buffer, { buffer = true, silent = true })

vim.keymap.set('n', 'ca', function()
  open_commit_buffer({'--amend'})
end, { buffer = true, silent = true })

vim.keymap.set('n', 'cf', ':<C-u>GCommitFixup ', { buffer = true })
vim.keymap.set('n', '<:', '<Plug>(gin-action-restore:ours)', { buffer = true })
vim.keymap.set('n', '>:', '<Plug>(gin-action-restore:theirs)', { buffer = true })
vim.keymap.set('n', '==', '<Plug>(gin-action-reset)', { buffer = true })

force_show_stash_size()
