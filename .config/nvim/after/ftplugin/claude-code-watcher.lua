local function close_claude_code_tab()
  ---@type ClaudeCodeWatchersTabState | nil
  local tab_state = vim.b.tab_state
  if tab_state == nil then
    vim.notify('vim.b.tab_state is nil', vim.log.levels.ERROR)
    return
  end

  local current_tab = vim.api.nvim_get_current_tabpage()
  if tab_state[current_tab] == nil then
    error('This tab is not a Claude Code watchers tab')
  end

  local jobs = tab_state[current_tab]
  if jobs == nil then
    error('This tab is not a Claude Code watchers tab')
  end

  vim.fn.jobstop(jobs.ccusage_job)
  vim.fn.jobstop(jobs.claude_monitor_job)
  vim.b.tab_state = nil
  vim.cmd('tabclose')
end

vim.keymap.set('n', 'Q', close_claude_code_tab, { buffer = true, desc = 'Close the Claude Code watchers tab' })

-- Stop this tab's watchers when leaving the tab,
-- and resume them when entering the tab
vim.api.nvim_create_autocmd({ 'TabLeave', 'TabEnter' }, {
  group = vim.api.nvim_create_augroup('FtpluginClaudeCodeWatcher', { clear = true }),
  callback = function(ev)
    ---@type ClaudeCodeWatchersTabState | nil
    local tab_state = vim.b.tab_state
    if tab_state == nil then
      vim.notify('vim.b.tab_state is nil', vim.log.levels.ERROR)
      return
    end

    if ev.event == 'TabEnter' then
      -- 再開
      vim.system({ 'kill', '-CONT ', tostring(tab_state.ccusage_pid) }):wait()
      vim.system({ 'kill', '-CONT ', tostring(tab_state.claude_monitor_pid) }):wait()
    else
      -- 停止
      vim.system({ 'kill', '-STOP ', tostring(tab_state.ccusage_pid) }):wait()
      vim.system({ 'kill', '-STOP ', tostring(tab_state.claude_monitor_pid) }):wait()
    end
  end,
})
