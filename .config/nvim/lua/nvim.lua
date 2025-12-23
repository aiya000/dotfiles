---Functions for init.lua and Neovim.
---関数がNeovimに関する事柄を意図しているか、もしくは`vim.*`に依存している場合は、こっち。
---それ以外の汎用的な関数は`./utils/functions.lua`に。

local fn = require('utils.functions')
local list = require('utils.list')

local s = fn.s

local M = {}

-- TODO: readonlyするとうまく動かないかも？
-- M.escaping_keys = fn.readonly({ '<Esc>', '<C-[>', '<C-l>' })
M.escaping_keys = { '<Esc>', '<C-[>', '<C-l>' }

---Validates `validatee` with `schema`.
---Thrown error Handled by `vim.notify()` if thrown.
---@generic T
---@param schema chotto.Schema<T>
function M.ensure(schema, validatee)
  schema:ensure(validatee, function(e)
    vim.notify('Validation failed: ' .. e, vim.log.levels.ERROR)
  end)
end

---@see M.reload_module
function M.reload_modules(...)
  return vim.iter({...}):map(M.reload_module):totable()
end

---TODO: ちゃんと変更を検知できてない。でもリロードはできてるっぽい
---Removes `module_name` from `package.loaded` and `require(module_name)` again
---@param module_name string
function M.reload_module(module_name)
  local older = require(module_name)
  package.loaded[module_name] = nil
  local newer = require(module_name)

  vim.schedule(function()
    if fn.deep_equal(older, newer) then
      vim.notify(("[ReloadModule] No changes: '%s'"):format(module_name), vim.log.levels.INFO)
    else
      vim.notify(("[ReloadModule] Changed: '%s'"):format(module_name), vim.log.levels.INFO)
    end
  end)

  return newer
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

---Storkes a stroke starts with normal mode.
---Also Can storke with virtual keymaps, compared to vim.cmd('normal stroke'), like `<Plug>(foo-bar)` and `viw<Plug>(foo-bar)p`.
---(NOTE: `vim.cmd('normal foo')` cannot handle `<Plug>(foo-bar)` correctly.)
---@param keys string --Like `<Plug>(foo-bar)`, `viw<Plug>(foo-bar)p`
function M.run_with_virtual_keymaps(keys)
  vim.fn.feedkeys(vim.api.nvim_replace_termcodes(keys, true, false, true))
end

---Replaces keys to key codes and do `vim.api.nvim_feedkeys()` it
---@param keys string
---@param mode? string --Default to 'n'. Expected to 'n', 'i', and etc. See `:h nvim_feedkeys()`
function M.feedkeys(keys, mode)
  mode = mode or 'n'
  vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes(keys, true, false, true), mode, true)
end

---Sets same mapping to multiple keys
---@param mode string | string[]
---@param keys string[]
---@param mapping string | fun(opts: table)
---@param opts? vim.keymap.set.Opts
function M.keymaps_set(mode, keys, mapping, opts)
  for _, key in ipairs(keys) do
    vim.keymap.set(mode, key, mapping, opts)
  end
end

-- TODO: readonlyするとなぜかうまく動かない。readonlyの実装がおかしい？
-- M.hl_groups = fn.readonly({
M.hl_groups = ({
  ErrorMsg = 'ErrorMsg', -- Red
  WarningMsg = 'WarningMsg', -- Yellow
  MoreMsg = 'MoreMsg', -- Green
  Question = 'Question', -- ?
  Normal = 'Normal', -- No color
})

---@param message string
---@param hl_group? string
---@param opts? { only_a_char: boolean }
---@return string
function M.confirm_to_get_charstr(message, hl_group, opts)
  opts = opts or { only_a_char = false }
  hl_group = hl_group or M.hl_groups.Question

  vim.api.nvim_echo({
    { message, hl_group },
  }, false, {})
  vim.cmd('redraw')

  return opts.only_a_char
    and vim.fn.nr2char(vim.fn.getchar())
    or vim.fn.getcharstr()
end

M.confirm = M.confirm_to_get_charstr

---@param message string
---@param opts? { only_a_char: boolean }
---@return string
function M.prompt(message, opts)
  return M.confirm_to_get_charstr(message, M.hl_groups.Normal, opts)
end

---(nui.nvim required.)
---@param message string
---@param opts? { only_a_char: boolean } -- Currently only `only_a_char = true` is supported
---@return string
function M.prompt_rich(message, opts)
  opts = opts or { only_a_char = false }
  if not opts.only_a_char then
    error('Currently, only `only_a_char = true` is supported')
  end

  local popup = require('nui.popup')({
    position = '50%',
    size = {
      width = 40,
      height = 3,
    },
    zindex = 100,
    enter = true,
    relative = 'editor',
    border = {
      style = 'rounded',
      text = { top = message },
    },
    win_options = {
      winblend = 10,
      winhighlight = 'Normal:Normal,FloatBorder:InitLuaSimpleGreen',
    },
  })

  return fn.try_finally(function()
    popup:mount()
    -- TODO: border-topじゃなくて、popup内にメッセージを表示するようにする
    return vim.fn.nr2char(vim.fn.getchar())
  end, function()
    popup:unmount()
  end)
end

---@return string | nil
local function whoami()
  local result = vim.system({ 'whoami' }):wait()
  if result.code ~= 0 then
    return nil
  end
  return vim.fn.trim(result.stdout)
end

---Runs `:source (neovim_home)/after/ftplugin/{filetype}.lua`
---@param filetype string
function M.source_after_ftplugin(filetype)
  vim.cmd(('execute "source" "%s"'):format(
    vim.fn.stdpath('config') .. '/after/ftplugin/' .. filetype .. '.lua'
  ))
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
---@param force? boolean --If true, removes trailing spaces even in excluded filetypes. Default is false
---@param range? [integer, integer] --If specified, removes trailing spaces in the range. Default is all lines.
function M.remove_trailing_spaces(force, range)
  force = force == true

  local excluded_filetypes = {
    'markdown',
  }
  if not force and list.has(excluded_filetypes, vim.bo.filetype) then
    vim.notify(('Removing trailing spaces in %s: Skipped'):format(vim.bo.filetype), vim.log.levels.WARN)
    return
  end
  local curpos = vim.fn.getcurpos()

  local range_str = '%'
  if range ~= nil then
    range_str = ('%d,%d'):format(range[1], range[2])
  end

  -- Use vim.cmd with string.format to avoid escaping issues
  -- Match spaces, tabs, and carriage returns at line end
  vim.cmd(([[%s s/[ \t\r]\+$//ge]]):format(range_str))

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
      env = vim.tbl_extend('keep', {
        NEOVIM_TERMINAL = true,
        NVIM_PARENT_ADDRESS = vim.v.servername, -- See '~/.dotfiles/.sh_generic/aliases/neovim.sh' and '~/.dotfiles/bash-toys/sources/nvim-parent-edit.sh'
      }, opts.env or {}),
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

function M.close_all_popups()
  for _, window in ipairs(vim.api.nvim_list_wins()) do
    local config = vim.api.nvim_win_get_config(window)
    if config.relative ~= '' then -- If it is a popup window
      vim.api.nvim_win_close(window, false)
    end
  end
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
  vim.cmd('MadoScratchBufferOpen md sp ' .. full_size)
  local output = vim.fn.execute(cmd)
  local lines = vim.split(output, '\n')
  vim.api.nvim_buf_set_lines(0, 0, -1, false, lines)
  vim.cmd('normal! gg"zdd')
end

---surround operations
local function get_current_obj_keys()
  local surround_blocks = vim.g['operator#surround#blocks'] or {}
  local surrounds = surround_blocks['-'] or {}
  local filetype_surrounds = surround_blocks[vim.bo.filetype] or {}

  -- Combine default and filetype-specific surrounds
  local all_surrounds = vim.list_extend(
    vim.deepcopy(surrounds), -- TODO: なんかここ、operatorで使ってるのに、オーダー高くない？
    filetype_surrounds
  )

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
    local char = vim.fn.nr2char(vim.fn.getchar())
    if list.has(InitLua.canceler_keys_for_my_operator_surround, char) then
      -- if calnceled
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

---LSP診断を新しいウィンドウで表示
---@param current_lsp_diagnostic vim.Diagnostic
local function open_lsp_diagnostic_detail(current_lsp_diagnostic)
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

  -- キーマッピングを追加
  vim.keymap.set('n', 'Q', '<cmd>bdelete!<CR>', { buffer = true, silent = true })
end

---Opens diagnostic detail in a new window (LSP or ALE)
function M.open_diagnostic_detail()
  local current_line = vim.fn.line('.')
  local lsp_diagnostics = vim.diagnostic.get(0, {
    lnum = current_line - 1, -- LSPは0-based
  })

  -- 現在行のLSP診断があるか確認
  local current_lsp_diagnostic = nil ---@type vim.Diagnostic | nil
  for _, diag in ipairs(lsp_diagnostics) do
    if diag.lnum == current_line - 1 then
      current_lsp_diagnostic = diag
      break
    end
  end

  -- LSP診断がない場合はALEの詳細を、そうでなければLSP診断の詳細を表示
  if current_lsp_diagnostic == nil then
    vim.cmd('ALEDetail')
  else
    open_lsp_diagnostic_detail(current_lsp_diagnostic)
  end
end

---Toggle LSP diagnostic virtual text
function M.toggle_diagnostic_virtual_text()
  local current_config = vim.diagnostic.config()
  local new_virtual_text = not current_config.virtual_text
  vim.diagnostic.config({ virtual_text = new_virtual_text })
  print('LSP virtual text: ' .. (new_virtual_text and 'enabled' or 'disabled'))
end

local CopilotTerm = nil
function M.toggle_copilot_cli()
  if CopilotTerm == nil then
    CopilotTerm = require('toggleterm.terminal').Terminal:new({
      cmd = ([[
        copilot
          --allow-tool write
          --allow-tool 'shell(notifu-respond)'
          --allow-tool 'shell(notifu.exe)'
          --allow-tool 'shell(git log)'
          --allow-tool 'shell(git show)'
          --allow-tool 'shell(git diff)'
          --allow-tool 'shell(git status)'
          --allow-tool 'shell(git reflog)'
      ]]):gsub('\r?\n', ' '),
      hidden = true,
      direction = 'float',
      on_open = function(_)
        -- Enter insert mode when the terminal opens
        vim.cmd('startinsert!')
      end,
    })
  end
  CopilotTerm:toggle()
end

local GeminiTerm = nil
function M.toggle_gemini_cli()
  if GeminiTerm == nil then
    GeminiTerm = require('toggleterm.terminal').Terminal:new({
      cmd = 'gemini',
      hidden = true,
      direction = 'float',
      on_open = function(_)
        -- Enter insert mode when the terminal opens
        vim.cmd('startinsert!')
      end,
    })
  end
  GeminiTerm:toggle()
end

---Clears flash.nvim highlights
function M.clear_flash_nvim_highlight()
  require('flash').toggle(false)

  -- Clear flash extmarks from all buffers
  for _, buf in ipairs(vim.api.nvim_list_bufs()) do
    if vim.api.nvim_buf_is_valid(buf) then
      pcall(vim.api.nvim_buf_clear_namespace, buf, vim.api.nvim_create_namespace('flash'), 0, -1)
    end
  end
end

---Clear LuaSnip snippet jump positions and highlights
function M.clear_luasnip()
  local ok, luasnip = pcall(require, 'luasnip')
  -- TODO: 各nodeに対して全てクリアする（全てのマーカーをクリアする）
  if ok and luasnip.session and luasnip.session.current_nodes[vim.api.nvim_get_current_buf()] then
    luasnip.unlink_current()
  end
end

function M.clear()
  M.close_all_popups()
  require('notify').dismiss({ silent = true, pending = true })
  vim.cmd.nohlsearch()
  pcall(vim.cmd, 'Winsep enable') -- Restore Winsep when Winsep is enabled
  M.clear_luasnip()
  pcall(M.clear_flash_nvim_highlight)
end

function M.restart_lsp_related_with_current_buffer()
  local excluded_lsp_servers = {
    'GitHub Copilot', -- TODO: 効いてない？
  }

  -- Clear all diagnostics for current buffer (including virtual text)
  vim.diagnostic.reset(nil, 0)

  local clients = vim.lsp.get_clients({ bufnr = 0 })
  for _, client in ipairs(clients) do
    if not vim.list_contains(excluded_lsp_servers, client.name) then
      -- Toggle
      vim.lsp.enable(client.name, false)
      vim.lsp.enable(client.name, true)
    end
  end
end


function M.clear_highlight_deeply()
  print('clearing...')
  M.clear()
  pcall(M.restart_lsp_related_with_current_buffer)
  print('cleared!')
end

function M.clear_highlight_and_write()
  M.clear()
  vim.cmd('write')
end

---今のところ、cmdpalette.nvimで入力をハックする用
---See 'cmdpalette.nvim' section in './plugins.lua'
---See 'cmdpalette.nvimと連携するための...' section in './autocmds.lua'
---@param keymaps table<string, string | (fun(): string)>
---@param input string --Like '::'
---@param set_current_line? fun(line: string): nil --Default is `vim.api.nvim_set_current_line()`. Also can select like `vim.fn.setcmdline()` or another functions
function M.replace_line(keymaps, input, set_current_line)
  local rhs = keymaps[input]
  if rhs == nil then
    return
  end

  local is_keys, keys = type(rhs) == 'string', rhs
  if is_keys then
    vim.api.nvim_set_current_line('')
    set_current_line(keys)
    return
  end

  local is_func, func = type(rhs) == 'function', rhs
  if is_func then
    vim.api.nvim_set_current_line('')
    set_current_line(func())
    return
  end

  error('replace_line(): rhs is neither string nor function: ' .. vim.inspect(rhs))
end

---@param local_dir string -- Assuming this is a realpath (not a relative path)
local function generate_helptags_when_existing_doc(local_dir)
  local local_doc_dir = local_dir .. '/doc'
  if vim.fn.isdirectory(local_doc_dir) == 1 then
    vim.cmd('helptags ' .. local_doc_dir)
  end
end

---Determines whether to load a plugin from a local directory or a remote repository
---@param remote_repo string
---@param local_dir string
---@param should_load_from_local boolean
---@param lazynvim_plugin_config? table --LazyPlugin
---```lua
---nvim.load_from_local_or_remote(
---  'aiya000/nvim-luasnip-emoji',
---  '~/Repository/nvim-luasnip-emoji',
---  InitLua.disable_luasnip_emoji == true,
---  {
---    dependencies = {
---      'L3MON4D3/LuaSnip',
---    },
---  }
---),
---```
function M.load_from_local_or_remote(
  remote_repo,
  local_dir,
  should_load_from_local,
  lazynvim_plugin_config
)
  local_dir = vim.fn.expand(local_dir) -- Make '~/somepath/nvim-foo' to realpath
  if should_load_from_local and not vim.fn.isdirectory(local_dir) then
    local message = ([[
      A plugin directory not found: %s
      Use remote repository instead.
    ]]):format(local_dir)
    vim.notify(message, vim.log.levels.ERROR)
    return M.load_from_local_or_remote(remote_repo, local_dir, false, lazynvim_plugin_config)
  end

  local base_config = nil
  if should_load_from_local then
    base_config = { dir = local_dir }
    generate_helptags_when_existing_doc(local_dir)
  else
    base_config = { remote_repo }
  end
  return vim.tbl_extend('keep', base_config, lazynvim_plugin_config or {})
end

local luasnippets_dir = vim.fn.stdpath('config') .. '/lua/luasnippets'

---Finds `*.lua` files in sub directories of the given directory
---@return string[] -- filetypes
---NOTE: サブディレクトリのスニペット（例: `typescript/{typescript,eslint,jest}.lua`）は、親スニペット（前述の例では`typescript.lua`）で読み込む。
local function find_luasnip_file_names()
  local handler = vim.uv.fs_scandir(luasnippets_dir)
  if handler == nil then
    error('Failed to scan directory: ' .. luasnippets_dir)
  end

  local result = {} ---@type string[]
  while true do
    local file_name, type = vim.uv.fs_scandir_next(handler)
    if file_name == nil then
      break
    elseif type == 'file' then
      table.insert(result, file_name)
    elseif type == 'directory' then
      -- continue
    else
      error('Not suported file type: ' .. vim.inspect({ type = type, file_name = file_name }))
    end
  end

  return result
end

---TODO: 全てのスニペットファイルが`{ snippets: LuaSnip[] }`でなく直接`LuaSnip[]`を返すようにして、この関数が必要ないようにする -- LuaSnipプラグインに型は実装されていないので、ここでLuaSnip型は擬似的な型名
local function load_luasnip(filetype, snips)
  local luasnip = require('luasnip')
  if type(snips.snippets) == 'table' then
    luasnip.add_snippets(filetype, snips.snippets)
    return
  elseif type(snips) == 'table' then
    luasnip.add_snippets(filetype, snips)
    return
  end
  vim.notify('Invalid snippet file: filetype =  ' .. filetype, vim.log.levels.ERROR)
end

---Clears all loaded modules matching the given prefix pattern
---@param target_module string -- e.g., 'luasnippets'
local function clear_module_cache(target_module)
  local prefix = target_module .. '.'
  for module_name, _ in pairs(package.loaded) do
    if module_name:match('^' .. vim.pesc(prefix)) then
      package.loaded[module_name] = nil
    end
  end
end

---@param opts? { reload?: boolean } -- Run `M.reload_module()` before loading snippets when `reload` is true
function M.load_luasnips(opts)
  opts = opts or { reload = false }

  if opts.reload then
    require('luasnip').cleanup()
    clear_module_cache('luasnippets')
  end

  local snip_files = find_luasnip_file_names()
  for _, snip_file in ipairs(snip_files) do
    local filetype = snip_file:gsub('%.lua$', '')
    local submodule_name = 'luasnippets.' .. filetype

    local ok, snips = pcall(require, submodule_name)
    if ok then
      load_luasnip(filetype, snips)
    else
      vim.notify(('Failed to load snippets: "%s" - %s'):format(snip_file, snips), vim.log.levels.ERROR)
    end
  end
end

function M.reload_luasnips()
  M.load_luasnips({ reload = true })
end

---Similar to `vim.v.argv`, but only returns files (not options and $0. $0 is usually 'nvim')
---```shell-session
---$ nvim
---:lua = nvim.arg_files() -- returns {}
---
---$ nvim somefile.txt
---:lua = nvim.arg_files() -- returns { 'somefile.txt' }
---
---$ nvim foo.txt bar.txt
---:lua = nvim.arg_files() -- returns { 'foo.txt', 'bar.txt' }
---
---$ nvim --headless somefile.txt +qa
---:lua = nvim.arg_files() -- returns { 'somefile.txt' } - Unlike `vim.v.argv` ( `nvim`, `--headless`, and `+qa` omitted)
---```
---@return string[]
function M.arg_files()
  return vim.iter(vim.v.argv)
    :filter(function(arg)
      return vim.loop.fs_stat(vim.fn.fnamemodify(arg, ':p')) ~= nil
    end)
    :totable()
end

---Checks if quickfix window is open and closes it
---@return boolean true if quickfix was open and closed, false otherwise
function M.close_quickfix_if_open()
  for _, wininfo in ipairs(vim.fn.getwininfo()) do
    if wininfo.quickfix == 1 then
      vim.cmd('cclose')
      return true
    end
  end
  return false
end

return M
