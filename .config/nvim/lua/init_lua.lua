-- Functions for init.lua

local list_util = require('utils.list')
local msg_util = require('utils.message')
local fn = require('utils.functions')

local M = {}

-- Allows to reuse `self`.
-- Params:
--   self: A
--   f: (self: A) -> B
-- Result: B
-- Example:
--   join(a:stdout, '')->vimrc#let({ result ->
--     \ result ==# foo
--       \ ? bar
--       \ : result
--    \ })
function M.let(self, f)
  return f(self)
end

-- Applies `f` if `p(value)`.
-- To re-use a:value.
-- Params:
--   value: A
--   p: (value: A) -> Bool
--   f: (value: A) -> B
-- Result: A | B
function M.apply_if(value, p, f)
  return M.let(value, function(val)
    return p(val) and f(val) or val
  end)
end

function M.identity(x)
  return x
end

-- Returns alt if f() throws an exception.
function M.catch(f, alt)
  local ok, result = pcall(f)
  return ok and result or alt
end

---@param install_dir_path string --ここのディレクトリパスにdein.vimをgit-cloneする
function M.install_dein_if_not_installed(install_dir_path)
  if vim.fn.exists('*dein#begin') ~= 0 then
    return
  end

  if vim.fn.executable('git') == 0 then
    error('git required to install dein.vim, but git is not executable.')
  end

  print('Installing dein.vim...')
  local result = vim.system(
    {'git', 'clone', 'https://github.com/Shougo/dein.vim', install_dir_path},
    { text = true }
  ):wait()

  if result.code ~= 0 then
    print('Failed to install dein.vim. Result:')
    fn.print_table(result)
    error('Exit.')
  end
  print('Done!')
end

-- Compresses continuously spaces to a space.
function M.compress_spaces()
  local recent_pattern = vim.fn.getreg('/')
  vim.cmd('try | s/\\s\\+/ /g | execute "normal! ==" | finally | let @/ = "' .. recent_pattern .. '" | endtry')
  vim.cmd('nohlsearch')
end

-- Removes trailing spaces of all lines.
function M.remove_trailing_spaces()
  local recent_pattern = vim.fn.getreg('/')
  local curpos = vim.fn.getcurpos()

  local ok = pcall(function()
    vim.cmd('%s/\\s*\\?$//g')
  end)

  if not ok then
    print('nothing todo')
  end

  vim.fn.setreg('/', recent_pattern)
  vim.fn.setpos('.', curpos)
end

-- Toggles diffthis and diffoff with some keymappings.
function M.toggle_diff()
  if vim.wo.diff then
    vim.cmd('diffoff')
    -- NOTE: This restores [c and ]c of .vimrc,
    -- Please see [c, ]c, [ale-previous], and [ale-next] in .vimrc.
    vim.keymap.set('n', '[c', '[ale-previous]', { buffer = true })
    vim.keymap.set('n', ']c', '[ale-next]', { buffer = true })
  else
    vim.cmd('diffthis')
    -- Get original
    vim.keymap.set('n', '[c', '[c', { buffer = true })
    vim.keymap.set('n', ']c', ']c', { buffer = true })
  end
  vim.cmd('set diff?')
end

-- Closes buffers of a specified filetypes.
function M.bufclose_filetype(filetypes)
  local closed = false
  for w = 1, vim.fn.winnr('$') do
    local buf_ft = vim.fn.getwinvar(w, '&filetype')
    if list_util.has(filetypes, buf_ft) then
      vim.cmd(':' .. w .. 'wincmd w')
      vim.cmd('quit')
      closed = true
    end
  end
  return closed
end

-- Toggles a file explorer
function M.toggle_explorer(path)
  path = path or vim.fn.expand('%:p:h')
  local closed = M.bufclose_filetype({ 'dirvish' })
  if not closed then
    M.open_explorer('vsplit', path)
  end
end

function M.open_explorer(split, path)
  path = path or vim.fn.expand('%:p:h')
  local cmd = split == 'stay' and ':Dirvish'
    or split == 'split' and ':split | silent Dirvish'
    or split == 'vsplit' and ':vsplit | silent Dirvish'
    or split == 'tabnew' and ':tabnew | silent Dirvish'
    or error('an unexpected way to open the explorer: ' .. split)

  if vim.fn.isdirectory(path) == 0 then
    -- :silent to ignore an error message. Because opening seems success.
    vim.cmd('silent ' .. cmd)
    return
  end

  vim.cmd(cmd .. ' ' .. path)
end

-- Fetches a detail of <title> from a URL
function M.get_webpage_title(url)
  local ok, result = pcall(function()
    print('fetching now...')
    return vim.fn.system(string.format('curl --silent %s | pup --plain "title json{}" | jq -r ".[0].text"', url))
  end)

  if not ok then
    return 'vimrc#get_webpage_title(): something happened: ' .. result
  end

  return result
end

-- :quit if only a window is existent.
-- :hide otherwise.
function M.hide_or_quit()
  local tabnum = vim.fn.tabpagenr('$')
  local winnum = vim.fn.tabpagewinnr(vim.fn.tabpagenr(), '$')
  if tabnum == 1 and winnum == 1 then
    vim.cmd('quit')
  else
    vim.cmd('hide')
  end
end

function M.toggle_ale_at_buffer()
  vim.b.ale_enabled = not (vim.b.ale_enabled or true)
  -- Refresh the state
  vim.cmd('ALEToggle')
  vim.cmd('ALEToggle')
end

-- Toggles indent-guides
function M.toggle_indent_guides()
  vim.g['vimrc#indent_guides_enable'] = not (vim.g['vimrc#indent_guides_enable'] or true)
  vim.cmd('IndentGuidesToggle')
end

-- Puts a register as stdin into the terminal buffer.
function M.put_as_stdin(detail)
  local current_bufnr = vim.fn.bufnr('%')
  vim.defer_fn(function()
    vim.fn.term_sendkeys(current_bufnr, detail)
  end, 0)
  return 'i'
end

-- Moves a current buffer to left of tab.
function M.move_window_forward()
  local tabwin_num = #vim.fn.tabpagebuflist()
  vim.cmd('mark Z')
  vim.cmd('hide')
  if tabwin_num ~= 1 then
    vim.cmd('tabnext')
  end
  vim.cmd('vsp')
  vim.cmd("normal! 'Z")

  if vim.fn.foldlevel('.') > 0 then
    vim.cmd('normal! zO')
  end
  vim.cmd('normal! zz')
end

-- Moves a current buffer to right of tab.
function M.move_window_backward()
  vim.cmd('mark Z')
  vim.cmd('hide')
  vim.cmd('tabprevious')
  vim.cmd('vsp')
  vim.cmd("normal! 'Z")

  if vim.fn.foldlevel('.') > 0 then
    vim.cmd('normal! zO')
  end
  vim.cmd('normal! zz')
end

-- Moves tab to left.
function M.move_tab_prev()
  if vim.fn.tabpagenr() == 1 then
    vim.cmd('$tabmove')
  else
    vim.cmd('tabmove -1')
  end
end

-- Moves tab to right.
function M.move_tab_next()
  if vim.fn.tabpagenr() == vim.fn.tabpagenr('$') then
    vim.cmd('0tabmove')
  else
    vim.cmd('+tabmove')
  end
end

-- Moves the cursor position to the last position of a file.
function M.visit_past_position()
  local past_posit = vim.fn.line('"')
  if past_posit > 0 and past_posit <= vim.fn.line('$') then
    vim.cmd('normal! g`"')
  end
end

-- Renames a file name of the current buffer.
function M.rename_to(new_name)
  local this_file = vim.fn.fnameescape(vim.fn.expand('%'))
  local new_name_esc = vim.fn.fnameescape(new_name)

  if vim.fn.fnamemodify(this_file, ':t') == new_name then
    msg_util.error('New name is same old name, operation abort')
    return
  end

  local file_editing = vim.bo.modified
  if file_editing then
    msg_util.error('Please :write this file')
    return
  end

  local new_file = vim.fn.fnamemodify(this_file, ':h') .. '/' .. new_name
  local failed = vim.fn.rename(this_file, new_file)
  if failed ~= 0 then
    msg_util.error(string.format('Rename %s to %s is failed', this_file, new_file))
    return
  end

  vim.cmd('edit ' .. new_file)
  vim.cmd('silent write')
  vim.cmd('silent bdelete ' .. this_file)

  print(string.format('Renamed %s to %s', this_file, new_file))
end

-- Get current buffer directory with fallback
function M.get_current_buffer_dir(options)
  options = options or {}
  local dir = vim.bo.buftype ~= 'terminal' and vim.bo.buftype ~= 'nofile' and vim.fn.expand('%:p:h')
    or vim.g.vimrc.git_root

  local alt_dir = options.alt_dir
  if dir and dir ~= '' then
    return dir
  elseif alt_dir then
    return alt_dir
  else
    error('The current buffer directory does not exist and an alter directory is not specified')
  end
end

-- Shows a popup window by vim.notify with good options
function M.popup_atcursor(messages)
  vim.notify(messages, vim.log.levels.INFO)
end

-- Export functions for backward compatibility with vim function calls
_G.vimrc = M

return M
