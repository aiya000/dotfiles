---nvim.lua のサブセット。lightweight版から使われる関数のみ定義する。
---nvim.lua はこのモジュールを require して再エクスポートする。

local fn = require('utils.functions')
local list = require('utils.list')

local s = fn.s

local M = {}

M.escaping_keys = { '<Esc>', '<C-[>', '<C-l>' }

M.special_chars = {
  ctrl_u = vim.api.nvim_replace_termcodes('<C-u>', true, false, true),
}

function M.run_with_virtual_keymaps(keys)
  vim.fn.feedkeys(vim.api.nvim_replace_termcodes(keys, true, false, true))
end

M.feedkeys_with_replace_termcodes = M.run_with_virtual_keymaps

function M.feedkeys(keys, mode)
  mode = mode or 'n'
  vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes(keys, true, false, true), mode, true)
end

function M.keymaps_set(mode, keys, mapping, opts)
  for _, key in ipairs(keys) do
    vim.keymap.set(mode, key, mapping, opts)
  end
end

function M.make_directory_if_missing(dir)
  local is_directory_existent = vim.fn.isdirectory(dir) == 0
  if is_directory_existent then
    return
  end
  local user = vim.env.USER or (function()
    local r = vim.system({ 'whoami' }):wait()
    return r.code == 0 and vim.fn.trim(r.stdout) or nil
  end)()
  if user == nil then
    error('Both $USER and `whoami` are not provided')
  end
  vim.fn.mkdir(dir, 'p', '755')
end

function M.compress_spaces()
  vim.cmd('s/\\s\\+/ /g')
  vim.cmd('execute "normal! =="')
  vim.cmd('nohlsearch')
end

local function create_removing_trailing_spaces(range)
  return function()
    local curpos = vim.fn.getcurpos()
    fn.try_finally(function()
      local range_str = range == nil and '%' or ('%d,%d'):format(range[1], range[2])
      vim.cmd(([[%s s/[ \t\r]\+$//ge]]):format(range_str))
    end, function()
      vim.fn.setpos('.', curpos)
    end)
  end
end

function M.remove_trailing_spaces(force, range)
  force = force == true
  local excluded_filetypes = { 'markdown' }
  local apply = create_removing_trailing_spaces(range)
  if not force and list.has(excluded_filetypes, vim.bo.filetype) then
    vim.ui.select({ 'Yes', 'No' }, { prompt = ('Trailing spaces in %s: Apply?'):format(vim.bo.filetype) }, function(choice)
      if choice == 'Yes' then apply() end
    end)
    return
  end
  apply()
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

function M.move_window_forward()
  local tabwin_num = #vim.fn.tabpagebuflist()
  vim.cmd('mark Z')
  vim.cmd('hide')
  if tabwin_num ~= 1 then vim.cmd('tabnext') end
  vim.cmd('vsp')
  vim.cmd("normal! 'Z")
  if vim.fn.foldlevel('.') > 0 then vim.cmd('normal! zO') end
  vim.cmd('normal! zz')
end

function M.move_window_backward()
  vim.cmd('mark Z')
  vim.cmd('hide')
  vim.cmd('tabprevious')
  vim.cmd('vsp')
  vim.cmd("normal! 'Z")
  if vim.fn.foldlevel('.') > 0 then vim.cmd('normal! zO') end
  vim.cmd('normal! zz')
end

function M.move_tab_prev()
  if vim.fn.tabpagenr() == 1 then
    vim.cmd('$tabmove')
  else
    vim.cmd('tabmove -1')
  end
end

function M.move_tab_next()
  if vim.fn.tabpagenr() == vim.fn.tabpagenr('$') then
    vim.cmd('0tabmove')
  else
    vim.cmd('+tabmove')
  end
end

function M.remove_text_after_cursor()
  local cmdpos = vim.fn.getcmdpos()
  local cmdline = vim.fn.getcmdline()
  if cmdpos < 2 then
    return ''
  else
    return cmdline:sub(1, cmdpos - 2)
  end
end

function M.close_all_popups()
  for _, window in ipairs(vim.api.nvim_list_wins()) do
    local success, config = pcall(vim.api.nvim_win_get_config, window)
    if success and config.relative ~= '' then
      vim.api.nvim_win_close(window, false)
    end
  end
end

function M.close_quickfix_if_open()
  for _, wininfo in ipairs(vim.fn.getwininfo()) do
    if wininfo.quickfix == 1 then
      vim.cmd('cclose')
      return true
    end
  end
  return false
end

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

function M.read_current_buffer_dir()
  if vim.bo.buftype == 'nofile' then return nil end
  local file_dir = vim.fn.expand('%:p:h')
  if file_dir ~= '' then return file_dir end
  return nil
end

function M.is_in_float_window()
  return vim.api.nvim_win_get_config(0).relative ~= ''
end

function M.toggle_diff()
  if vim.wo.diff then
    vim.cmd('diffoff')
    vim.keymap.set('n', '[c', '[ale-previous]', { buffer = true })
    vim.keymap.set('n', ']c', '[ale-next]', { buffer = true })
  else
    vim.cmd('diffthis')
    vim.keymap.set('n', '[c', '[c', { buffer = true })
    vim.keymap.set('n', ']c', ']c', { buffer = true })
  end
  vim.cmd('set diff?')
end

function M.toggle_diagnostic_virtual_text()
  local current_config = vim.diagnostic.config()
  local new_virtual_text = not current_config.virtual_text
  vim.diagnostic.config({ virtual_text = new_virtual_text })
  print('LSP virtual text: ' .. (new_virtual_text and 'enabled' or 'disabled'))
end

function M.goto_diagnostic(direction)
  local diag_method = direction == 'next'
      and { lsp_func = vim.diagnostic.goto_next, ale_cmd = 'ALENext' }
    or { lsp_func = vim.diagnostic.goto_prev, ale_cmd = 'ALEPrevious' }
  local current_line = vim.fn.line('.')
  local lsp_moved = false
  local ok, _ = pcall(diag_method.lsp_func, { float = { border = 'rounded' } })
  if ok and vim.fn.line('.') ~= current_line then lsp_moved = true end
  if not lsp_moved then vim.cmd(diag_method.ale_cmd) end
end

function M.open_diagnostic_detail()
  local current_line = vim.fn.line('.')
  local lsp_diagnostics = vim.diagnostic.get(0, { lnum = current_line - 1 })
  local current_lsp_diagnostic = nil
  for _, diag in ipairs(lsp_diagnostics) do
    if diag.lnum == current_line - 1 then
      current_lsp_diagnostic = diag
      break
    end
  end
  if current_lsp_diagnostic == nil then
    vim.cmd('ALEDetail')
  else
    local content = {
      '# LSP Diagnostic', '',
      '**Severity:** ' .. vim.diagnostic.severity[current_lsp_diagnostic.severity],
      '**Source:** ' .. (current_lsp_diagnostic.source or 'LSP'),
      '**Line:** ' .. (current_lsp_diagnostic.lnum + 1),
      '**Column:** ' .. (current_lsp_diagnostic.col + 1),
      '', '**Message:**',
    }
    for _, line in ipairs(vim.split(current_lsp_diagnostic.message, '\n')) do
      table.insert(content, line)
    end
    vim.cmd('new')
    vim.api.nvim_buf_set_lines(0, 0, -1, false, content)
    vim.bo.buftype = 'nofile'
    vim.bo.bufhidden = 'wipe'
    vim.bo.filetype = 'markdown'
    vim.bo.readonly = true
    vim.bo.modifiable = false
    vim.wo.wrap = true
    vim.wo.linebreak = true
    vim.cmd('resize ' .. math.min(#content + 2, math.floor(vim.o.lines * 0.4)))
    vim.keymap.set('n', 'Q', '<cmd>bdelete!<CR>', { buffer = true, silent = true })
  end
end

function M.open_buffer_in_float_window(buf, opts)
  opts = opts or {}
  local enter = opts.enter ~= false
  local width = math.floor(vim.o.columns * 0.85)
  local height = math.floor(vim.o.lines * 0.85)
  return vim.api.nvim_open_win(buf, enter, {
    relative = 'editor',
    width = width, height = height,
    row = math.floor((vim.o.lines - height) / 2),
    col = math.floor((vim.o.columns - width) / 2),
    style = 'minimal', border = 'rounded',
  })
end

function M.get_selected_text()
  return vim.fn.getregion(vim.fn.getpos('v'), vim.fn.getpos('.'), { type = vim.fn.mode() })
end

local function get_termopen_options_bdelete_when_on_exit(opts)
  return function(job_id, exit_code, event_type)
    if (opts or {}).on_exit ~= nil then opts.on_exit(job_id, exit_code, event_type) end
    vim.cmd('bdelete!')
  end
end

local function setup_start_insert_after_paste(bufnr)
  for _, key in ipairs({ 'p', 'P' }) do
    vim.keymap.set('n', key, key .. '<Cmd>startinsert<CR>', { buffer = bufnr })
  end
end

function M.termopen_temporary(cmd, opts)
  opts = vim.tbl_extend('force', {}, opts or {})
  local start_insert_after_paste = opts.start_insert_after_paste
  opts.start_insert_after_paste = nil
  local opts_with_on_exit = vim.tbl_extend('force', opts, {
    term = true,
    on_exit = get_termopen_options_bdelete_when_on_exit(opts),
  })
  vim.fn.jobstart(cmd, opts_with_on_exit)
  if start_insert_after_paste then
    setup_start_insert_after_paste(vim.api.nvim_get_current_buf())
  end
end

function M.is_using_windows_git()
  return vim.system({ 'git', '--version' }):wait().stdout:find('windows') ~= nil
end

function M.termopen_shell(opts, should_enter_insert_mode)
  opts = opts or {}
  should_enter_insert_mode = should_enter_insert_mode == nil
  M.termopen_temporary(
    vim.env.SHELL,
    vim.tbl_extend('force', opts, {
      start_insert_after_paste = true,
      env = vim.tbl_extend('keep', {
        NEOVIM_TERMINAL = not M.is_using_windows_git() or nil,
        NVIM_PARENT_ADDRESS = vim.v.servername,
      }, opts.env or {}),
    })
  )
  vim.opt_local.filetype = 'terminal-shell'
  if should_enter_insert_mode then vim.fn.feedkeys('i') end
end

local function make_cli_app_toggler(cmd, on_open_extra, env, opts)
  opts = opts or {}
  local term = nil
  return function()
    if term == nil then
      term = require('toggleterm.terminal').Terminal:new({
        cmd = cmd, hidden = true, direction = 'float', env = env,
        on_open = function(t)
          if on_open_extra then on_open_extra(t) end
          if opts.start_insert_after_paste then setup_start_insert_after_paste(t.bufnr) end
          vim.api.nvim_create_autocmd('BufEnter', {
            buffer = t.bufnr,
            callback = function() vim.schedule(function() vim.cmd('startinsert!') end) end,
          })
        end,
      })
    end
    term:toggle()
  end
end

M.toggle_copilot_cli = make_cli_app_toggler(
  ([[copilot --allow-tool write --allow-tool 'shell(notify)' --allow-tool 'shell(git log)' --allow-tool 'shell(git show)' --allow-tool 'shell(git diff)' --allow-tool 'shell(git status)' --allow-tool 'shell(git reflog)']]):gsub('\r?\n', ' '),
  function()
    vim.keymap.set('t', '<C-p>', '<Up>', { buffer = true })
    vim.keymap.set('t', '<C-n>', '<Down>', { buffer = true })
  end,
  nil,
  { start_insert_after_paste = true }
)
M.toggle_antigravity_cli = make_cli_app_toggler('agy', nil, nil, { start_insert_after_paste = true })
M.toggle_devin_cli = make_cli_app_toggler('devin', nil, nil, { start_insert_after_paste = true })
M.toggle_shell = make_cli_app_toggler(vim.env.SHELL, nil, {
  NEOVIM_TERMINAL = not M.is_using_windows_git() or nil,
  NVIM_PARENT_ADDRESS = vim.v.servername,
}, { start_insert_after_paste = true })

function M.clear_flash_nvim_highlight()
  require('flash').toggle(false)
  for _, buf in ipairs(vim.api.nvim_list_bufs()) do
    if vim.api.nvim_buf_is_valid(buf) then
      pcall(vim.api.nvim_buf_clear_namespace, buf, vim.api.nvim_create_namespace('flash'), 0, -1)
    end
  end
end

function M.clear_luasnip()
  local ok, luasnip = pcall(require, 'luasnip')
  if ok and luasnip.session and luasnip.session.current_nodes[vim.api.nvim_get_current_buf()] then
    luasnip.unlink_current()
  end
end

function M.clear()
  M.close_all_popups()
  require('notify').dismiss({ silent = true, pending = true })
  vim.cmd.nohlsearch()
  pcall(vim.cmd, 'Winsep enable')
  M.clear_luasnip()
  pcall(M.clear_flash_nvim_highlight)
end

function M.clear_highlight_deeply()
  print('clearing...')
  M.clear()
  print('cleared!')
end

function M.clear_highlight_and_write()
  M.clear()
  vim.cmd('write')
end

---surround operations (vim-operator-surround連携)
local function get_current_obj_keys()
  local surround_blocks = vim.g['operator#surround#blocks'] or {}
  local surrounds = surround_blocks['-'] or {}
  local filetype_surrounds = surround_blocks[vim.bo.filetype] or {}
  local all_surrounds = vim.list_extend(vim.deepcopy(surrounds), filetype_surrounds)
  local obj_keys = {}
  for _, surround in ipairs(all_surrounds) do
    if surround.keys then vim.list_extend(obj_keys, surround.keys) end
  end
  return obj_keys
end

local function input_obj_key_of(obj_keys, prompt)
  prompt = prompt or 'Surround key: '
  local stroke = ''
  while not vim.tbl_contains(obj_keys, stroke) do
    vim.cmd.echo(('"%s"'):format(prompt .. stroke))
    local char = vim.fn.nr2char(vim.fn.getchar())
    if char == M.special_chars.ctrl_u then
      stroke = ''
    elseif list.has(InitLua.canceler_keys_for_my_operator_surround, char) then
      return nil
    else
      stroke = stroke .. char
    end
  end
  return stroke
end

local function append_choose_surround(visualizer)
  local obj_keys = get_current_obj_keys()
  local obj_key = input_obj_key_of(obj_keys)
  if obj_key == nil then print('Cancelled'); return nil end
  M.run_with_virtual_keymaps(visualizer .. '<Plug>(operator-surround-append)' .. obj_key)
  return visualizer .. '\\<Plug>(operator-surround-append)' .. obj_key
end

function M.delete_mostly_inner_surround()
  local obj_keys = get_current_obj_keys()
  local obj_key = input_obj_key_of(obj_keys)
  if obj_key == nil then print('Cancelled'); return end
  M.run_with_virtual_keymaps('va' .. obj_key .. '<Plug>(operator-surround-delete)')
  vim.call('repeat#set', 'va' .. obj_key .. '\\<Plug>(operator-surround-delete)')
end

function M.replace_mostly_inner_surround()
  local obj_keys = get_current_obj_keys()
  local obj_key_from = input_obj_key_of(obj_keys, 'Replaced char: ')
  if obj_key_from == nil then print('Cancelled'); return end
  local obj_key_to = input_obj_key_of(obj_keys, 'Replacing char: ')
  if obj_key_to == nil then print('Cancelled'); return end
  M.run_with_virtual_keymaps('va' .. obj_key_from .. '<Plug>(operator-surround-replace)' .. obj_key_to)
  vim.call('repeat#set', 'va' .. obj_key_from .. '\\<Plug>(operator-surround-replace)' .. obj_key_to)
end

local function append_choose_surround_(visualizer)
  local stroked = append_choose_surround(visualizer)
  if stroked ~= nil then vim.call('repeat#set', stroked) end
end

function M.append_choose_surround_normal()
  append_choose_surround_('viw')
end

function M.append_choose_surround_wide()
  append_choose_surround_('viW')
end

function M.camelize_or_uncamelize_current_word_as_repeatable()
  M.run_with_virtual_keymaps('viw<Plug>(operator-camelize-toggle)')
  vim.call('repeat#set', 'viw\\<Plug>(operator-camelize-toggle)')
end

local function generate_helptags_when_existing_doc(local_dir)
  local local_doc_dir = local_dir .. '/doc'
  if vim.fn.isdirectory(local_doc_dir) == 1 then
    vim.cmd('helptags ' .. local_doc_dir)
  end
end

function M.load_from_local_or_remote(remote_repo, local_dir, should_load_from_local, lazynvim_plugin_config)
  local_dir = vim.fn.expand(local_dir)
  if should_load_from_local and not vim.fn.isdirectory(local_dir) then
    local message = ('A plugin directory not found: %s\nUse remote repository instead.'):format(local_dir)
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

local function find_luasnip_file_names()
  local handler = vim.uv.fs_scandir(luasnippets_dir)
  if handler == nil then error('Failed to scan directory: ' .. luasnippets_dir) end
  local result = {}
  while true do
    local file_name, type = vim.uv.fs_scandir_next(handler)
    if file_name == nil then break
    elseif type == 'file' then table.insert(result, file_name)
    elseif type == 'directory' then
    else error('Not suported file type: ' .. vim.inspect({ type = type, file_name = file_name }))
    end
  end
  return result
end

local function load_luasnip(filetype, snips)
  local luasnip = require('luasnip')
  if type(snips.snippets) == 'table' then
    luasnip.add_snippets(filetype, snips.snippets)
  elseif type(snips) == 'table' then
    luasnip.add_snippets(filetype, snips)
  else
    vim.notify('Invalid snippet file: filetype =  ' .. filetype, vim.log.levels.ERROR)
  end
end

local function clear_module_cache(target_module)
  local prefix = target_module .. '.'
  for module_name, _ in pairs(package.loaded) do
    if module_name:match('^' .. vim.pesc(prefix)) then
      package.loaded[module_name] = nil
    end
  end
end

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

function M.reload_modules(...)
  return vim.iter({ ... }):map(function(module_name)
    package.loaded[module_name] = nil
    return require(module_name)
  end):totable()
end

M.hl_groups = {
  ErrorMsg = 'ErrorMsg',
  WarningMsg = 'WarningMsg',
  MoreMsg = 'MoreMsg',
  Question = 'Question',
  Normal = 'Normal',
}

return M
