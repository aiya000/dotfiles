---Functions for init.lua and Neovim

local list = require('utils.list')
local fn = require('utils.functions')
local s = fn.s

local M = {}

---Storkes a stroke starts with normal mode.
---Also Can storke with virtual keymaps, compared to vim.cmd('normal stroke'), like `<Plug>(foo-bar)` and `viw<Plug>(foo-bar)p`.
---(NOTE: `vim.cmd('normal foo')` cannot handle `<Plug>(foo-bar)` correctly.)
---
---@param keystroke string --Like `<Plug>(foo-bar)`, `viw<Plug>(foo-bar)p`
---@return nil
function M.run_with_virtual_keymaps(keystroke)
  vim.fn.feedkeys(vim.api.nvim_replace_termcodes(keystroke, true, false, true))
end

---@param install_dir_path string --ここのディレクトリパスにdein.vimをgit-cloneする
function M.install_dein_if_not_installed(install_dir_path)
  if vim.fn.isdirectory(install_dir_path) == 1 then
    return
  end

  if vim.fn.executable('git') == 0 then
    error('git required to install dein.vim, but git is not executable.')
  end

  print('Installing dein.vim...')
  local result = vim
    .system({ 'git', 'clone', 'https://github.com/Shougo/dein.vim', install_dir_path }, { text = true })
    :wait()

  if result.code ~= 0 then
    print('Failed to install dein.vim. Result:')
    fn.print_table(result)
    error('Exit.')
  end
  print('Done!')
end

---@return string | nil
local function whoami()
  local result = vim.fn.system('whoami'):wait()
  if result.code ~= 0 then
    return nil
  end
  return result
end

---NOTE: This requires `$USER` or `whoami` command
---@param dir string
function M.make_directory_if_missing(dir)
  if vim.fn.isdirectory(dir) == 0 then
    local user = vim.env.USER or whoami()
    if user == nil then
      error('Both $USER and `whoami` are not provided')
    end
    local group = vim.env.GROUP or ''

    vim.fn.mkdir(dir, 'p', '755')
  end
end

---Compresses continuously spaces to a space.
function M.compress_spaces()
  local recent_pattern = vim.fn.getreg('/')
  vim.cmd(s('try | s/\\s\\+/ /g | execute "normal! ==" | finally | let @/ = "{recent_pattern}" | endtry'))
  vim.cmd('nohlsearch')
end

---Removes trailing spaces of all lines.
function M.remove_trailing_spaces()
  local curpos = vim.fn.getcurpos()
  vim.cmd(':%s/\\s*$//g')
  vim.fn.setpos('.', curpos)
end

---Toggles diffthis and diffoff with some keymappings.
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

---Closes buffers of a specified filetypes.
function M.bufclose_filetype(filetypes)
  local closed = false
  for w = 1, vim.fn.winnr('$') do
    local buf_ft = vim.fn.getwinvar(w, '&filetype')
    if vim.tbl_contains(filetypes, buf_ft) then
      vim.cmd(s(':{w}wincmd w'))
      vim.cmd('quit')
      closed = true
    end
  end
  return closed
end

---Toggles a file explorer
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
    or error(s('an unexpected way to open the explorer: {split}'))

  if vim.fn.isdirectory(path) == 0 then
    -- :silent to ignore an error message. Because opening seems success.
    vim.cmd(s('silent {cmd}'))
    return
  end

  vim.cmd(s('{cmd} {path}'))
end

---Fetches a detail of <title> from a URL
function M.get_webpage_title(url)
  local ok, result = pcall(function()
    print('fetching now...')
    return vim.fn.system(string.format('curl --silent %s | pup --plain "title json{}" | jq -r ".[0].text"', url))
  end)

  if not ok then
    return s('vimrc#get_webpage_title(): something happened: {result}', { result = result })
  end

  return result
end

---:quit if only a window is existent.
---:hide otherwise.
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

---Toggles indent-guides
function M.toggle_indent_guides()
  vim.g['vimrc#indent_guides_enable'] = not (vim.g['vimrc#indent_guides_enable'] or true)
  vim.cmd('IndentGuidesToggle')
end

---Puts a register as stdin into the terminal buffer.
function M.put_as_stdin(detail)
  local current_bufnr = vim.fn.bufnr('%')
  vim.defer_fn(function()
    vim.fn.term_sendkeys(current_bufnr, detail)
  end, 0)
  return 'i'
end

---Moves a current buffer to left of tab.
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

---Moves a current buffer to right of tab.
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

---Moves tab to left.
function M.move_tab_prev()
  if vim.fn.tabpagenr() == 1 then
    vim.cmd('$tabmove')
  else
    vim.cmd('tabmove -1')
  end
end

---Moves tab to right.
function M.move_tab_next()
  if vim.fn.tabpagenr() == vim.fn.tabpagenr('$') then
    vim.cmd('0tabmove')
  else
    vim.cmd('+tabmove')
  end
end

---Moves the cursor position to the last position of a file.
function M.visit_past_position()
  local past_posit = vim.fn.line('"')
  if past_posit > 0 and past_posit <= vim.fn.line('$') then
    vim.cmd('normal! g`"')
  end
end

---Renames a file name of the current buffer.
function M.rename_to(new_name)
  local this_file = vim.fn.fnameescape(vim.fn.expand('%'))
  local new_name_esc = vim.fn.fnameescape(new_name)

  if vim.fn.fnamemodify(this_file, ':t') == new_name then
    vim.notify('New name is same old name, operation abort', vim.log.levels.ERROR)
    return
  end

  local file_editing = vim.bo.modified
  if file_editing then
    vim.notify('Please :write this file', vim.log.levels.ERROR)
    return
  end

  local new_file = vim.fn.fnamemodify(this_file, ':h') .. '/' .. new_name
  local failed = vim.fn.rename(this_file, new_file)
  if failed ~= 0 then
    vim.notify(s('Rename {this_file} to {new_file} is failed'), vim.log.levels.ERROR)
    return
  end

  vim.cmd(s('edit {new_file}'))
  vim.cmd('silent write')
  vim.cmd(s('silent bdelete {this_file}'))

  print(s('Renamed {this_file} to {new_file}'))
end

---Gets current buffer directory with fallback
function M.get_current_buffer_dir(options)
  options = options or {}
  local dir = vim.bo.buftype ~= 'terminal' and vim.bo.buftype ~= 'nofile' and vim.fn.expand('%:p:h') or InitLua.git_root

  local alt_dir = options.alt_dir
  if dir and dir ~= '' then
    return dir
  elseif alt_dir then
    return alt_dir
  else
    error('The current buffer directory does not exist and an alter directory is not specified')
  end
end

---Shows a popup window at cursor with good options
---@param messages string|string[]
function M.popup_atcursor(messages)
  -- TODO: 微調整。atcursorになってないかもしれない。今はとりあえずなゆちゃんが出してくれたコードをそのまま使ってる
  vim.api.nvim_open_win(vim.api.nvim_create_buf(false, true), true, {
    relative = 'editor',
    width = 20,
    height = 3,
    row = 10,
    col = 10,
  })
end

function M.close_all_popups()
  for _, window in ipairs(vim.api.nvim_list_wins()) do
    local config = vim.api.nvim_win_get_config(window)
    if config.relative ~= '' then -- If it is a popup window
      vim.api.nvim_win_close(window, false)
    end
  end
end

---Opens a buffer with DeepL translation result
local function deepl_translate_open_buffer(result)
  vim.cmd('ScratchBufferOpenNext md sp')
  vim.cmd('put=' .. vim.fn.string(result))
  vim.cmd('normal! gg')
  vim.cmd('normal! "zdd')
end

---DeepL translation function with multiple output methods
function M.deepl_translate(line_count, start_line, end_line, target_lang, source_lang, methods)
  -- If range is not specified, translate the current line, or translate the specified range
  local lines
  if line_count == -1 then
    lines = { vim.fn.getline('.') }
  else
    lines = vim.fn.getline(start_line, end_line)
  end

  local put_by = {
    ---@param result string
    yank = function(result)
      vim.fn.setreg('"', result)
    end,
    ---@param result string
    echo = function(result)
      print(result) -- Equivalent to s:Msg.echo('Normal', result)
    end,
    ---@param result string
    buffer = function(result)
      deepl_translate_open_buffer(result)
    end,
  }

  local text = table.concat(lines, '\n')
  local result = vim.fn['deepl#translate'](text, target_lang, source_lang)

  for _, method in ipairs(methods) do
    if not put_by[method] then
      error('Unknown method: ' .. method)
    end
    put_by[method](result)
  end
end


function M.setup_operator_surround()
  -- Basic symbols excluding brackets () [] {} and ` for unique mappings
  local basic_symbols = {}
  vim.list_extend(basic_symbols, list.char_range('!', "'"))
  vim.list_extend(basic_symbols, { '*', '&', '_', '|', '~', ':', '/' })

  local basic_between = {}
  for _, char in ipairs(basic_symbols) do
    table.insert(basic_between, {
      block = { char, char },
      motionwise = { 'char', 'line', 'block' },
      keys = { char },
    })
  end

  local basic_html_tags = {
    { block = { '<p>', '</p>' }, motionwise = { 'char' }, keys = { '[p' } },
    { block = { '<a>', '</a>' }, motionwise = { 'char' }, keys = { '[a' } },
    { block = { '<div>', '</div>' }, motionwise = { 'char' }, keys = { '[d' } },
    { block = { '<span>', '</span>' }, motionwise = { 'char' }, keys = { '[s' } },
    { block = { '<h1>', '</h1>' }, motionwise = { 'char' }, keys = { '[h1' } },
    { block = { '<h2>', '</h2>' }, motionwise = { 'char' }, keys = { '[h2' } },
    { block = { '<h3>', '</h3>' }, motionwise = { 'char' }, keys = { '[h3' } },
    { block = { '<h4>', '</h4>' }, motionwise = { 'char' }, keys = { '[h4' } },
    { block = { '<h5>', '</h5>' }, motionwise = { 'char' }, keys = { '[h5' } },
    { block = { '<ol>', '</ol>' }, motionwise = { 'char' }, keys = { '[ol' } },
    { block = { '<ul>', '</ul>' }, motionwise = { 'char' }, keys = { '[ul' } },
    { block = { '<li>', '</li>' }, motionwise = { 'char' }, keys = { '[li' } },
  }

  -- Set operator#surround#blocks configuration
  vim.g['operator#surround#blocks'] = {
    ['-'] = vim.list_extend({
      { block = { '(', ')' }, motionwise = { 'char', 'line', 'block' }, keys = { '(', ')', 'p' } },
      { block = { '[', ']' }, motionwise = { 'char', 'line', 'block' }, keys = { ']', 'k' } },
      { block = { '{', '}' }, motionwise = { 'char', 'line', 'block' }, keys = { '{', '}', 'P' } },
      { block = { '<', '>' }, motionwise = { 'char', 'line', 'block' }, keys = { '<', '>', 'K' } },
      { block = { ' ', ' ' }, motionwise = { 'char', 'line', 'block' }, keys = { '  ' } },
      { block = { '`', '`' }, motionwise = { 'char', 'line', 'block' }, keys = { '`', 'b' } },
      { block = { '（', '）' }, motionwise = { 'char', 'line', 'block' }, keys = { 'jp' } },
      { block = { '「', '」' }, motionwise = { 'char', 'line', 'block' }, keys = { 'jk' } },
      { block = { '【', '】' }, motionwise = { 'char', 'line', 'block' }, keys = { 'jK' } },
      { block = { '『', '』' }, motionwise = { 'char', 'line', 'block' }, keys = { 'j-k' } },
    }, basic_between),
    review = {
      { block = { '@<b>{', '}' }, motionwise = { 'char' }, keys = { 'B' } },
      { block = { '@<i>{', '}' }, motionwise = { 'char' }, keys = { 'i' } },
      { block = { '@<u>{', '}' }, motionwise = { 'char' }, keys = { 'u' } },
      { block = { '@<tt>{', '}' }, motionwise = { 'char' }, keys = { 't' } },
      { block = { '@<idx>{', '}' }, motionwise = { 'char' }, keys = { 'x' } },
      { block = { '@<ruby>{', ', ruby}' }, motionwise = { 'char' }, keys = { 'r' } },
      { block = { '@<code>{', '}' }, motionwise = { 'char' }, keys = { 'c' } },
      { block = { '@<mathcode>{', '}' }, motionwise = { 'char' }, keys = { 'm' } },
      { block = { '@<img>{', '}' }, motionwise = { 'char' }, keys = { '[i' } },
      { block = { '@<list>{', '}' }, motionwise = { 'char' }, keys = { '[l' } },
    },
    markdown = {
      { block = { '**', '**' }, motionwise = { 'char', 'block' }, keys = { 'B' } },
      { block = { '~~', '~~' }, motionwise = { 'char', 'block' }, keys = { '~' } },
    },
    html = basic_html_tags,
    vue = basic_html_tags,
    ['typescript.tsx'] = basic_html_tags,
  }
end

---Kills from cursor to end of command line
function M.remove_text_after_cursor()
  local cmdpos = vim.fn.getcmdpos()
  local cmdline = vim.fn.getcmdline()
  if cmdpos < 2 then
    return ''
  else
    return cmdline:sub(1, cmdpos - 2)
  end
end

function M.tabnext_loop()
  if vim.fn.tabpagenr() == vim.fn.tabpagenr('$') then
    vim.cmd('tabnext 1')
  else
    vim.cmd('tabnext')
  end
end

function M.tabprev_loop()
  if vim.fn.tabpagenr() == 1 then
    vim.cmd('tablast')
  else
    vim.cmd('tabprevious')
  end
end

function M.open_buffer_to_execute(cmd)
  local full_size = 100
  -- Use ScratchBufferOpen command similar to scratch_buffer#open
  vim.cmd('ScratchBufferOpen md sp ' .. full_size)
  
  -- Execute the command and capture output
  local output = vim.fn.execute(cmd)
  
  -- Put the output into the buffer
  vim.cmd('put=' .. vim.fn.string(output))
  
  -- Go to beginning and delete first 2 empty lines
  vim.cmd('normal! gg2dd')
end

---surround operations
local function get_current_obj_keys()
  local surround_blocks = vim.g['operator#surround#blocks'] or {}
  local surrounds = surround_blocks['-'] or {}
  local filetype_surrounds = surround_blocks[vim.bo.filetype] or {}

  -- Combine default and filetype-specific surrounds
  local all_surrounds = vim.list_extend(vim.deepcopy(surrounds), filetype_surrounds)

  -- Extract all keys from surrounds
  local obj_keys = {}
  for _, surround in ipairs(all_surrounds) do
    if surround.keys then
      vim.list_extend(obj_keys, surround.keys)
    end
  end
  return obj_keys
end

local function input_obj_key_of(obj_keys)
  local stroke = ''
  while not vim.tbl_contains(obj_keys, stroke) do
    local char = vim.fn.getchar()
    char = vim.fn.nr2char(char)

    -- Check for escape sequences (Esc, Ctrl-C, Ctrl-[)
    if char == '' or char == '' then
      return nil
    end
    stroke = stroke .. char
  end
  return stroke
end

---@param visualizer string --See append_choose_surround_()
---@return string | nil --Ran key stroke if not cancelled
local function append_choose_surround(visualizer)
  local obj_keys = get_current_obj_keys()
  local obj_key = input_obj_key_of(obj_keys)
  if obj_key == nil then
    print('Cancelled')
    return nil
  end

  M.run_with_virtual_keymaps(visualizer .. '<Plug>(operator-surround-append)' .. obj_key)
  return visualizer .. '\\<Plug>(operator-surround-append)' .. obj_key
end

function M.delete_mostly_inner_surround()
  local obj_keys = get_current_obj_keys()
  local obj_key = input_obj_key_of(obj_keys)
  if obj_key == nil then
    print('Cancelled')
    return
  end

  M.run_with_virtual_keymaps('va' .. obj_key .. '<Plug>(operator-surround-delete)')
  vim.call('repeat#set', 'va' .. obj_key .. '\\<Plug>(operator-surround-delete)')
end

function M.replace_mostly_inner_surround()
  local obj_keys = get_current_obj_keys()

  local obj_key_from = input_obj_key_of(obj_keys)
  if obj_key_from == nil then
    print('Cancelled')
    return
  end

  local obj_key_to = input_obj_key_of(obj_keys)
  if obj_key_to == nil then
    print('Cancelled')
    return
  end

  M.run_with_virtual_keymaps('va' .. obj_key_from .. '<Plug>(operator-surround-replace)' .. obj_key_to)
  vim.call('repeat#set', 'va' .. obj_key_from .. '\\<Plug>(operator-surround-replace)' .. obj_key_to)
end

---@param visualizer string --To select text, like 'viw' or 'viW'
local function append_choose_surround_(visualizer)
  local stroked = append_choose_surround(visualizer)
  if stroked ~= nil then
    vim.call('repeat#set', stroked)
  end
end

function M.append_choose_surround_normal()
  append_choose_surround_('viw')
end

function M.append_choose_surround_wide()
  append_choose_surround_('viW')
end

-- Camelize or uncamelize current word with repeat support
function M.camelize_or_uncamelize_current_word_as_repeatable()
  -- Use vim-operator-camelize plugin to toggle camelCase/snake_case
  M.run_with_virtual_keymaps('viw<Plug>(operator-camelize-toggle)')
  vim.call('repeat#set', 'viw\\<Plug>(operator-camelize-toggle)')
end

return M
