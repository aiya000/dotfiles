---Functions for init.lua and Neovim.
---関数がNeovimに関する事柄を意図しているか、もしくは`vim.*`に依存している場合は、こっち。
---それ以外の汎用的な関数は`./utils/functions.lua`に。

local list = require('utils.list')
local fn = require('utils.functions')
local c = require('chotto')

local s = fn.s

local M = {}

---Removes `module_name` from `package.loaded` and `require(module_name)` again
---@param module_name string
---@param starts_with_only boolean
function M.reload_module(module_name, starts_with_only)
  local older = require(module_name)
  require('plenary.reload').reload_module(module_name, starts_with_only)
  local newer = require(module_name)

  vim.schedule(function()
    if fn.deep_equal(older, newer) then
      print(('[ReloadModule] No changes: %s'):format(module_name))
    else
      print(('[ReloadModule] Reloaded: %s'):format(module_name))
    end
  end)
end

---Almost same as `vim.cmd`, but typed as a function
---```lua
----- pcall(vim.cmd, 'SomeComand') -- This is type mismatched, because vim.cmd is a table
---pcall(vim_cmd, 'SomeComand') -- This is OK
---```
---@param cmd string
function M.vim_cmd(cmd)
  vim.cmd(cmd)
end

---Returns a function to add autocmds related to augroup named by `group_name`
---```lua
---local add_autocmd = helper.create_adding_autocmd('InitLuaHelper')
---add_autocmd('VimEnter', function()
---  print('Neovim started.')
---end)
---add_autocmd('CmdwinEnter', function()
---  print('Cmdwin entered.')
---end)
---```
---@param group_name string
---@return fun(
---   event: string | string[],
---   callback: (fun(args: vim.api.keyset.create_autocmd.callback_args): nil) | string,
---   pattern_or_opts?: string | string[] | vim.api.keyset.create_autocmd,
---)
---@see vim.api.nvim_create_augroup()
---@see vim.api.nvim_create_autocmd()
---`helper.reload_module()`するときにautocmdsがクリアされるので、便利。
---（`augroup`を`clear = true`で生成しているため。）
function M.create_adding_autocmd(group_name)
  local augroup = vim.api.nvim_create_augroup(group_name, { clear = true })

  local function add_autocmd(event, callback, pattern_or_opts)
    local opts = { group = augroup, callback = callback }
    if pattern_or_opts == nil then
      vim.tbl_extend('keep', opts, {})
    end
    if type(pattern_or_opts) == 'string' or c.array(c.string()):safe_parse(pattern_or_opts) then
      vim.tbl_extend('keep', opts, { pattern = pattern_or_opts })
    end
    if type(pattern_or_opts) == 'table' then
      vim.tbl_extend('keep', opts, pattern_or_opts)
    end
    vim.api.nvim_create_autocmd(event, opts)
  end

  return add_autocmd
end

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
  local result = vim.system({ 'whoami' }):wait()
  if result.code ~= 0 then
    return nil
  end
  return vim.fn.trim(result.stdout)
end

---NOTE: This requires `$USER` or `whoami` command
---@param dir string
function M.make_directory_if_missing(dir)
  local is_directory_existent = vim.fn.isdirectory(dir) == 0
  if is_directory_existent then
    return
  end

  local user = vim.env.USER or whoami()
  if user == nil then
    error('Both $USER and `whoami` are not provided')
  end

  vim.fn.mkdir(dir, 'p', '755')
end

---Compresses continuously spaces to a space
function M.compress_spaces()
  vim.cmd('s/\\s\\+/ /g')
  vim.cmd('execute "normal! =="')
  vim.cmd('nohlsearch')
end

---Removes trailing spaces of all lines
function M.remove_trailing_spaces()
  local curpos = vim.fn.getcurpos()
  vim.cmd(':%s/\\s*$//g')
  vim.fn.setpos('.', curpos)
end

---Toggles diffthis and diffoff with some keymappings
function M.toggle_diff()
  if vim.wo.diff then
    vim.cmd('diffoff')
    -- NOTE: This restores [c and ]c of .vimrc,
    -- Please see [c, ]c, [ale-previous], and [ale-next] in .vimrc
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

---Closes buffers of a specified filetypes
function M.bufclose_filetype(filetypes)
  local closed = false
  for w = 1, vim.fn.winnr('$') do
    local buf_ft = vim.fn.getwinvar(w, '&filetype')
    if vim.tbl_contains(filetypes, buf_ft) then
      vim.cmd(s(':{w}wincmd w', { w = w }))
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
    or error('an unexpected way to open the explorer: ' .. split)

  if vim.fn.isdirectory(path) == 0 then
    -- :silent to ignore an error message. Because opening seems success
    vim.cmd('silent ' .. cmd)
    return
  end

  vim.cmd(cmd .. ' ' .. path)
end

---Creates `termopen_temporary()`'s default on_exit option.
---See `:h job-id` for what is job_id of the returned function.
---See `:h on_exit`  for what this returns.
---@param opts? table --See `:h jobstart()`
---@return fun(job_id: integer, exit_code: integer, event_type: 'exit'): nil
function M.get_termopen_options_bdelete_when_on_exit(opts)
  return function(job_id, exit_code, event_type)
    if (opts or {}).on_exit ~= nil then
      opts.on_exit(job_id, exit_code, event_type)
    end
    vim.cmd('bdelete!')
  end
end

---Opens a terminal buffer that closes it when exited immediately.
---See `:h jobstart()` about the params.
---@params cmd string
---@param opts? table
function M.termopen_temporary(cmd, opts)
  local opts_with_on_exit = vim.tbl_extend('force', opts or {}, {
    term = true,
    on_exit = M.get_termopen_options_bdelete_when_on_exit(opts),
  })
  vim.fn.jobstart(cmd, opts_with_on_exit)
end

---Opens a terminal buffer with $SHELL that closes it when exited immediately.
---@param opts? table --See `:h jobstart()`
---@param should_enter_insert_mode? boolean --Default is `true`
---@see get_termopen_options_bdelete_when_on_exit()
---@see termopen_temporary()
function M.termopen_shell(opts, should_enter_insert_mode)
  opts = opts or {}
  should_enter_insert_mode = should_enter_insert_mode == nil

  M.termopen_temporary(
    vim.env.SHELL,
    vim.tbl_extend('force', opts, {
      env = vim.tbl_extend('keep', { NEOVIM_TERMINAL = true }, opts.env or {}),
    })
  )
  vim.opt_local.filetype = 'terminal-shell'

  if should_enter_insert_mode then
    vim.fn.feedkeys('i')
  end
end

---Fetches a detail of <title> from a URL
function M.get_webpage_title(url)
  local ok, result = pcall(function()
    print('fetching now...')
    return vim.fn.system(string.format('curl --silent %s | pup --plain "title json{}" | jq -r ".[0].text"', url))
  end)

  return ok and result or 'get_webpage_title(): something happened: ' .. result
end

---Moves a current buffer to left of tab
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

---Moves a current buffer to right of tab
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

---Moves tab to left
function M.move_tab_prev()
  if vim.fn.tabpagenr() == 1 then
    vim.cmd('$tabmove')
  else
    vim.cmd('tabmove -1')
  end
end

---Moves tab to right
function M.move_tab_next()
  if vim.fn.tabpagenr() == vim.fn.tabpagenr('$') then
    vim.cmd('0tabmove')
  else
    vim.cmd('+tabmove')
  end
end

---Renames a file name of the current buffer
function M.rename_to(new_name)
  local this_file = vim.fn.fnameescape(vim.fn.expand('%'))
  local new_name_ = vim.fn.fnameescape(new_name)

  if vim.fn.fnamemodify(this_file, ':t') == new_name_ then
    vim.notify('New name is same old name, operation abort', vim.log.levels.ERROR)
    return
  end

  local file_editing = vim.bo.modified
  if file_editing then
    vim.notify('Please :write this file', vim.log.levels.ERROR)
    return
  end

  local new_file = vim.fn.fnamemodify(this_file, ':h') .. '/' .. new_name_
  local result_code = vim.fn.rename(this_file, new_file)
  if result_code ~= 0 then
    vim.notify(
      s('Rename {this_file} to {new_file} is failed', {
        this_file = this_file,
        new_file = new_file,
      }),
      vim.log.levels.ERROR
    )
    return
  end

  vim.cmd('edit ' .. new_file)
  vim.cmd('silent write')
  vim.cmd('silent bdelete ' .. this_file)

  vim.notify(
    s('Renamed {this_file} to {new_file}', {
      this_file = this_file,
      new_file = new_file,
    }),
    vim.log.levels.INFO
  )
end

---TODO: This is not working
---Reads a cwd of the current terminal buffer.
---Throws error if not in a terminal buffer or failed to get cwd.
---@return string
function M.get_term_shell_buffer_current_dir()
  if vim.b.terminal_job_id == nil then
    error('Not in a terminal buffer')
  end

  local pid = vim.fn.jobpid(vim.b.terminal_job_id)
  if pid <= 0 then
    error('Failed to get a pid of the terminal job')
  end

  local cwd = vim.fn.readlink(('/proc/%d/cwd'):format(pid))
  if cwd == nil or cwd == '' then
    error('Failed to get a cwd of the terminal job')
  end

  return cwd
end

---Reads the current buffer directory
---@returns string | nil --Returns nil if failed to get the directory
function M.read_current_buffer_dir()
  if vim.bo.buftype == 'nofile' then
    return nil
  end

  if vim.bo.buftype == 'terminal' then
    local ok, result = pcall(M.get_term_shell_buffer_current_dir)
    return ok and result or nil
  end

  local file_dir = vim.fn.expand('%:p:h')
  if file_dir ~= '' then
    -- NOTE: nofileでなく、`file_dir == ''`なんてことある？
    return file_dir
  end

  return nil
end

---translate.nvimのfloat windowを`close_all_popups()`で閉じると、
---次回実行時にエラーになるので、カーソル移動を使って閉じる。
---@see M.close_all_popups()
function M.move_cursor_and_reset()
  local pos = vim.fn.getpos('.')
  vim.cmd('normal! l')
  vim.fn.setpos('.', pos)
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

  local html_blocks = {
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

  local markdown_blocks = vim.fn.extendnew(html_blocks, {
    { block = { '**', '**' }, motionwise = { 'char', 'block' }, keys = { 'B' } },
    { block = { '~~', '~~' }, motionwise = { 'char', 'block' }, keys = { '~' } },
    { block = { '<b>', '</b>' }, motionwise = { 'char' }, keys = { '[b' } },
  })

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
    lua = {
      { block = { '[[', ']]' }, motionwise = { 'char', 'line', 'block' }, keys = { '[', ']' } },
    },
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
    markdown = markdown_blocks,
    html = html_blocks,
    vue = html_blocks,
    ['typescript.tsx'] = html_blocks,
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

---@param direction 'next' | 'previous'
local function get_diagnostic_method(direction)
  return direction == 'next' and { lsp_func = vim.diagnostic.goto_next, ale_cmd = 'ALENext' }
    or direction == 'previous' and { lsp_func = vim.diagnostic.goto_prev, ale_cmd = 'ALEPrevious' }
    or error('Invalid direction: ' .. tostring(direction))
end

---LSP診断での移動を試行。
---LSPで移動できなかった場合はALEで移動。
---@param direction 'next' | 'previous'
function M.goto_diagnostic(direction)
  local goto = get_diagnostic_method(direction)
  local current_line = vim.fn.line('.')
  local lsp_moved = false

  local ok, _ = pcall(goto.lsp_func, { float = { border = 'rounded' } })
  if ok and vim.fn.line('.') ~= current_line then
    lsp_moved = true
  end

  if not lsp_moved then
    vim.cmd(goto.ale_cmd)
  end
end

---Opens diagnostic detail in a new window (LSP or ALE)
function M.open_diagnostic_detail()
  local current_line = vim.fn.line('.')
  local lsp_diagnostics = vim.diagnostic.get(0, {
    lnum = current_line - 1, -- LSPは0-based
  })

  -- 現在行のLSP診断があるか確認
  local current_lsp_diagnostic = nil
  for _, diag in ipairs(lsp_diagnostics) do
    if diag.lnum == current_line - 1 then
      current_lsp_diagnostic = diag
      break
    end
  end

  if not current_lsp_diagnostic then
    -- LSP診断がない場合はALEの詳細を表示
    vim.cmd('ALEDetail')
    return
  end

  -- LSP診断を新しいウィンドウで表示
  local content = {}
  table.insert(content, '# LSP Diagnostic')
  table.insert(content, '')
  table.insert(content, '**Severity:** ' .. vim.diagnostic.severity[current_lsp_diagnostic.severity])
  table.insert(content, '**Source:** ' .. (current_lsp_diagnostic.source or 'LSP'))
  table.insert(content, '**Line:** ' .. (current_lsp_diagnostic.lnum + 1))
  table.insert(content, '**Column:** ' .. (current_lsp_diagnostic.col + 1))
  table.insert(content, '')
  table.insert(content, '**Message:**')

  -- メッセージを行ごとに分割
  local message_lines = vim.split(current_lsp_diagnostic.message, '\n')
  for _, line in ipairs(message_lines) do
    table.insert(content, line)
  end

  -- 新しいバッファを作成
  vim.cmd('new')
  vim.api.nvim_buf_set_lines(0, 0, -1, false, content)
  vim.bo.buftype = 'nofile'
  vim.bo.bufhidden = 'wipe'
  vim.bo.filetype = 'markdown'
  vim.bo.readonly = true
  vim.bo.modifiable = false
  vim.wo.wrap = true
  vim.wo.linebreak = true

  -- ウィンドウサイズを調整
  local height = math.min(#content + 2, math.floor(vim.o.lines * 0.4))
  vim.cmd('resize ' .. height)
end

function M.open_claude_code_watchers()
  -- Open watchers
  vim.cmd('tabnew')
  local ccusage_job = vim.fn.jobstart({ 'ccusage', 'blocks', '--live' }, { term = true })
  vim.opt_local.filetype = 'claude-code-watcher' -- See `after/ftplugin/claude-code-watcher.lua` for keymaps and autocmds
  vim.cmd('vertical new')
  local claude_monitor_job = vim.fn.jobstart(
    { 'claude-monitor', '--plan', 'pro', '--timezone', 'Asia/Tokyo' },
    { term = true }
  )
  vim.opt_local.filetype = 'claude-code-watcher'

  ---@type ClaudeCodeWatchersTabState
  vim.b.tab_state = {
    ccusage_job = ccusage_job,
    claude_monitor_job = claude_monitor_job,
    ccusage_pid = vim.fn.jobpid(ccusage_job),
    claude_monitor_pid = vim.fn.jobpid(claude_monitor_job),
  }
end

function M.show_lsp_diagnostic_float()
  vim.diagnostic.open_float(nil, {
    focusable = false,
    close_events = { 'BufLeave', 'CursorMoved', 'InsertEnter', 'FocusLost' },
    border = 'rounded',
    source = 'always',
    prefix = ' ',
    scope = 'cursor',
  })
end

local gemini_term = nil
vim.schedule(function()
  gemini_term = require('toggleterm.terminal').Terminal:new({
    cmd = 'gemini',
    hidden = true,
    direction = 'float',
    on_open = function(_)
      -- Enter insert mode when the terminal opens
      vim.cmd('startinsert!')
    end,
  })
end)

function M.toggle_gemini_cli()
  if gemini_term == nil then
    error('toggle_gemini_cli(): gemini_term has never initialized yet')
  end
  gemini_term:toggle()
end

function M.clear_flash_nvim_highlight()
  require('flash').toggle(false)

  -- Clear flash extmarks from all buffers
  for _, buf in ipairs(vim.api.nvim_list_bufs()) do
    if vim.api.nvim_buf_is_valid(buf) then
      pcall(vim.api.nvim_buf_clear_namespace, buf, vim.api.nvim_create_namespace('flash'), 0, -1)
    end
  end

  -- Force redraw to clear any remaining visual artifacts
  vim.cmd('redraw!')
end

return M
