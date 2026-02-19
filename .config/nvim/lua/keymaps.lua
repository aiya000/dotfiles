local fn = require('utils.functions')
local git_log = require('git-log')
local nvim = require('nvim')
local network = require('utils.network')
local telescope = require('telescope.builtin')
local snip = require('luasnip')

local s = fn.s

-- normal mode {{{

-- Allow keymaps like <C-c>{foo}, and {bar}<C-c>
vim.keymap.set('n', '<C-c>', '<NOP>')
vim.keymap.set('n', '<C-c><C-c>', '<C-c>')

-- Allow keymaps like <C-g>{foo}, and {bar}<C-g>
vim.keymap.set('n', '<C-g>', '<NOP>')
vim.keymap.set('n', '<C-g><C-g>', '<C-g>')

-- Other than below
vim.keymap.set('n', '<CR>', 'o<Esc>')
nvim.keymaps_set('n', { '<C-j>', '<C-m>' }, '<CR>', { remap = true })
nvim.keymaps_set('n', { '<C-[>', '<Esc>', '<C-l>' }, nvim.clear, { silent = true })
vim.keymap.set('n', '<C-k><C-l>', nvim.clear_highlight_deeply)
vim.keymap.set('n', '<C-k>o', '<Cmd>e! %<CR>', { silent = true })
vim.keymap.set('n', 'gG', 'ggVG')
vim.keymap.set('n', "'gG", 'ggVG"+y')
vim.keymap.set('n', 'q:', ':') -- `:`でcmdpaletteを開くので逆に`q:`でcmd-modeを開く
vim.keymap.set('n', '(', '(zv')
vim.keymap.set('n', ')', ')zv')
vim.keymap.set('n', '<C-k><C-j>', nvim.clear_highlight_and_write)
vim.keymap.set('n', '<C-k>J', '<Cmd>wall | echo "written all !"<CR>', { silent = true })
vim.keymap.set('n', '<C-]>', 'g<C-]>')
vim.keymap.set('n', '<leader>q', '<Cmd>copen<CR>', { silent = true })
vim.keymap.set('n', '<leader><leader>q', '<Cmd>cclose<CR>', { silent = true })
vim.keymap.set('n', 'Y', 'yg_')
vim.keymap.set('n', 'g<C-]>', '<C-]>')
vim.keymap.set('n', 'g_', '$')
vim.keymap.set('n', 'zs', 'zszh')
vim.keymap.set('n', '{', '{zv')
vim.keymap.set('n', '}', '}zv')
vim.keymap.set('n', ',,', 'ggVG"+y<C-o>') -- Copy the buffer content to clipboard
vim.keymap.set('n', ',<', 'ggVG"_s') -- Refresh the buffer content
vim.keymap.set('n', '<C-x><C-n>', '<C-n>')
vim.keymap.set('n', '<C-x><C-p>', '<C-p>')
vim.keymap.set('n', '/', [[/\m]])
vim.keymap.set('n', '?', [[?\m]])

-- Search
local function try_show_search_number_or_do_nothing()
  local ok, hlslens = pcall(require, 'hlslens')
  if not ok then
    return
  end
  hlslens.start()
end

--- Search cword without moving
vim.keymap.set('n', 'g*', function()
  local pos = vim.fn.getpos('.')
  vim.cmd('silent! normal! *')
  vim.fn.setpos('.', pos)
  try_show_search_number_or_do_nothing()
end)

--- Keep search direction
vim.keymap.set('n', 'n', function()
  vim.cmd('silent! normal! ' .. (vim.v.searchforward == 1 and 'nzv' or 'Nzv'))
  try_show_search_number_or_do_nothing()
end)

vim.keymap.set('n', 'N', function()
  vim.cmd('silent! normal! ' .. (vim.v.searchforward == 1 and 'Nzv' or 'nzv'))
  try_show_search_number_or_do_nothing()
end)

vim.keymap.set('n', '*', function()
  vim.cmd('silent! normal! *zv')
  try_show_search_number_or_do_nothing()
end)

vim.keymap.set('n', '#', function()
  vim.cmd('silent! normal! #zv')
  try_show_search_number_or_do_nothing()
end)

vim.keymap.set('n', 'Q', function()
  if nvim.close_quickfix_if_open() then
    return
  end

  local closing_target_buffer_filetype = {
    'diff',
    'gin-branch',
    'gin-log',
    'gin-status',
    'git-log',
    'git-show',
    'netrw',
    'quickrun',
  }
  nvim.bufclose_filetype(closing_target_buffer_filetype)
end)

-- Foldings
vim.keymap.set('n', 'h', function()
  return vim.fn.foldclosed('.') > -1 and 'zo' or 'h'
end, { expr = true })

vim.keymap.set('n', 'l', function()
  return vim.fn.foldclosed('.') > -1 and 'zo' or 'l'
end, { expr = true })

vim.keymap.set('n', 'zj', 'zjzo')
vim.keymap.set('n', 'zk', 'zkzo[zzt')

vim.keymap.set('n', 'zC', 'zM') -- なんとなくこっちの方が僕の直感にあってた
vim.keymap.set('n', 'zA', 'zR') --

-- Windows, buffers, and tabs
vim.keymap.set('n', '<Space>h', '<C-w>h')
vim.keymap.set('n', '<Space>j', '<C-w>j')
vim.keymap.set('n', '<Space>k', '<C-w>k')
vim.keymap.set('n', '<Space>l', '<C-w>l')

vim.keymap.set('n', 'ghR', '<C-w>r')
vim.keymap.set('n', 'ghq', '<Cmd>q<CR>', { silent = true })
vim.keymap.set('n', 'ghQ', '<Cmd>quitall<CR>', { silent = true })
vim.keymap.set('n', 'ghc', '<Cmd>bdelete<CR>', { silent = true })
vim.keymap.set('n', 'ghC', '<Cmd>bdelete!<CR>', { silent = true })
vim.keymap.set('n', 'gho', '<Cmd>only<CR>', { silent = true })
vim.keymap.set('n', 'gh_', '<Cmd>resize<CR>', { silent = true })
vim.keymap.set('n', 'gh"', '<Cmd>resize 5<CR>', { silent = true })
vim.keymap.set('n', "gh'", '<Cmd>resize 10<CR>', { silent = true })
vim.keymap.set('n', 'gh|', '<C-w>|')
vim.keymap.set('n', 'gh\\', '<Cmd>vertical resize 1<CR>', { silent = true })
vim.keymap.set('n', 'gh%', '<Cmd>vertical resize 20<CR>', { silent = true })
vim.keymap.set('n', 'gh=', '<C-w>=')
vim.keymap.set('n', 'gh+', 'gh_gh|', { remap = true })
vim.keymap.set('n', 'ghH', '<C-w>H')
vim.keymap.set('n', 'ghJ', '<C-w>J')
vim.keymap.set('n', 'ghK', '<C-w>K')
vim.keymap.set('n', 'ghL', '<C-w>L')
vim.keymap.set('n', 'ghs', '<Cmd>split<CR>', { silent = true })
vim.keymap.set('n', 'ghv', '<Cmd>vsplit<CR>', { silent = true })

vim.keymap.set('n', 'gH', 'mZ:tabnew<CR>`Z', { silent = true })
vim.keymap.set('n', 'ghh', 'mZ:hide<CR>:tabnew<CR>`Z', { silent = true })
vim.keymap.set('n', 'ght', '<Cmd>tabclose<CR>', { silent = true })

vim.keymap.set('n', '<C-w>q', '<NOP>')
vim.keymap.set('n', '<C-w>c', '<NOP>')
vim.keymap.set('n', '<C-w>r', '<NOP>')
vim.keymap.set('n', '<C-w>_', '<NOP>')
vim.keymap.set('n', '<C-w>\\', '<NOP>')
vim.keymap.set('n', '<C-w>=', '<NOP>')
vim.keymap.set('n', '<C-w>o', '<NOP>')
vim.keymap.set('n', '<C-w>H', '<NOP>')
vim.keymap.set('n', '<C-w>J', '<NOP>')
vim.keymap.set('n', '<C-w>K', '<NOP>')
vim.keymap.set('n', '<C-w>L', '<NOP>')
vim.keymap.set('n', '<C-w>s', '<NOP>')
vim.keymap.set('n', '<C-w>v', '<NOP>')
vim.keymap.set('n', 'gh', '<NOP>')

--- Window navigations
vim.keymap.set('n', '<C-s>N', function()
  nvim.move_window_forward()
end, { silent = true })

vim.keymap.set('n', '<C-s>P', function()
  nvim.move_window_forward()
end, { silent = true })

--- Tabs navigations
vim.keymap.set('n', '<C-n>', nvim.tabnext_loop, { silent = true })
vim.keymap.set('n', '<C-p>', nvim.tabprev_loop, { silent = true })

--- Start tab move mode with moving the current tab
vim.keymap.set('n', '<C-s>n', function()
  nvim.move_tab_next()
  if InitLua.hydra.tab_move == nil then
    vim.notify('InitLua.hydra.tab_move is not loaded', vim.log.levels.ERROR)
  else
    InitLua.hydra.tab_move:activate()
  end
end)

vim.keymap.set('n', '<C-s>p', function()
  nvim.move_tab_prev()
  if InitLua.hydra.tab_move == nil then
    vim.notify('InitLua.hydra.tab_move is not loaded', vim.log.levels.ERROR)
  else
    InitLua.hydra.tab_move:activate()
  end
end)

local function fallback_to_path_at_started()
  vim.api.nvim_echo(
    { { 'No directory found. Fallback to the path at started: ' .. InitLua.path_at_started } },
    false,
    {}
  )
  return InitLua.path_at_started
end

-- `termopen()`
vim.keymap.set('n', '<leader>v', function()
  local cwd = nvim.read_current_buffer_dir() or fallback_to_path_at_started()
  vim.cmd('vertical new')
  nvim.termopen_shell({ cwd = cwd })
end)

vim.keymap.set('n', '<leader><leader>v', function()
  local cwd = nvim.read_current_buffer_dir() or fallback_to_path_at_started()
  vim.cmd('new')
  nvim.termopen_shell({ cwd = cwd })
end)

--- Open in the current window
vim.keymap.set('n', '<leader>V', function()
  -- NOTE: 現在のウィンドウでlspのエラーなどがあると、タイミングによってその表示を持ち越してしまうので、新しいウィンドウで開く
  local cwd = nvim.read_current_buffer_dir() or fallback_to_path_at_started()
  local current_win = vim.api.nvim_get_current_win()
  vim.cmd('new')
  nvim.termopen_shell({ cwd = cwd }, false)
  vim.api.nvim_win_close(current_win, false)
  vim.fn.feedkeys('i')
end)

vim.keymap.set('n', '<leader><leader>V', function()
  local cwd = nvim.read_current_buffer_dir() or fallback_to_path_at_started()
  vim.cmd('tabnew')
  nvim.termopen_shell({ cwd = cwd })
end)

-- File explorer
vim.keymap.set('n', '<leader>e', '<Cmd>vsp|Oil<CR>', { silent = true })
vim.keymap.set('n', '<leader>E', '<Cmd>Oil<CR>', { silent = true })
vim.keymap.set('n', '<leader><leader>e', '<Cmd>sp|Oil<CR>', { silent = true })
vim.keymap.set('n', '<leader><leader>E', '<Cmd>tabnew|Oil<CR>', { silent = true })

-- List up
vim.keymap.set('n', '<C-k><C-e>', function()
  telescope.find_files({
    hidden = true,
    no_ignore = true,
    no_ignore_parent = true,
  })
end)

vim.keymap.set('n', '<C-k><C-f>', function()
  telescope.lsp_document_symbols({ symbols = { 'function' } })
end)

vim.keymap.set('n', '<C-k>f', telescope.treesitter)
vim.keymap.set('n', '<C-k><C-r>', telescope.reloader)
vim.keymap.set('n', 'L', telescope.buffers)
vim.keymap.set('n', 'H', telescope.live_grep)
vim.keymap.set('n', 'M', '<Cmd>Telescope frecency<CR>', { silent = true })
vim.keymap.set('n', 'm>', telescope.marks)
vim.keymap.set('n', 'q>', telescope.registers)
vim.keymap.set('n', 'y>', '<Cmd>Telescope yank_history<CR>', { silent = true })

vim.keymap.set('n', 'g>', function()
  nvim.open_buffer_to_execute('messages') -- This feature is not provided by telescope.nvim. use :messages instead
end, { silent = true })

--- Another AI Agents
vim.keymap.set('n', '<leader>gc', nvim.toggle_copilot_cli)
vim.keymap.set('n', '<leader>Gc', nvim.toggle_gemini_cli)

-- Options
vim.keymap.set('n', '<C-h><C-w>', '<Cmd>setlocal wrap! wrap?<CR>', { silent = true })
vim.keymap.set('n', '<C-h><C-c>', '<Cmd>setlocal cursorline! cursorline?<CR>', { silent = true })
vim.keymap.set('n', '<C-h>c', '<Cmd>setlocal cursorcolumn! cursorcolumn?<CR>', { silent = true })
vim.keymap.set('n', '<C-h><C-r>', '<Cmd>setlocal relativenumber! relativenumber?<CR>', { silent = true })
vim.keymap.set('n', '<C-h><C-l>', '<Cmd>setlocal list! list?<CR>', { silent = true })
vim.keymap.set('n', '<C-h><C-n>', '<Cmd>setlocal number! number?<CR>', { silent = true })
vim.keymap.set('n', '<C-h>v', nvim.toggle_diagnostic_virtual_text, { silent = true })
vim.keymap.set('n', '<C-h><C-d>', nvim.toggle_diff, { silent = true })

vim.keymap.set('n', '<C-h><C-f>', function()
  if vim.opt.foldmethod:get() == 'expr' then
    vim.opt.foldmethod = 'marker'
  else
    vim.opt.foldmethod = 'expr'
  end
  print(' foldmethod=' .. vim.opt.foldmethod:get())
end, { silent = true })

vim.keymap.set('n', '<C-h><C-v>', function()
  local verticaledit = vim.opt_local.virtualedit:get()
  vim.opt_local.virtualedit = (verticaledit[1] == '' or #verticaledit == 0) and 'all' or ''
  vim.cmd('set virtualedit?')
end, { silent = true })

-- Visualize a last pasted range
vim.keymap.set('n', 'gp', function()
  local reg = string.sub(vim.fn.getregtype(), 1, 1)
  return ('`[{%s}`]'):format(reg)
end, { expr = true })

-- Copy and paste
--- clipboard
vim.keymap.set('n', '<leader>p', '"+p')
vim.keymap.set('n', '<leader>P', '"+P')
vim.keymap.set('n', '<leader>y', '"+y')
vim.keymap.set('n', '<leader>Y', '"+yg_')
vim.keymap.set('n', '<leader>dd', '"+dd')
vim.keymap.set('n', '<leader>D', '"+D')
vim.keymap.set('n', '<leader>d', '"+d')
vim.keymap.set('n', '<leader>x', '"+x')

---Puts the current file path relative to the git root to buffer
---@param put_to 'above' | 'bellow'
local function put_filepath_relative_to_git_root(put_to)
  local current_filepath = vim.system({ 'git', 'ls-files', '--full-name', vim.fn.expand('%') }):wait()
  if current_filepath.code ~= 0 then
    vim.notify(current_filepath.stderr, vim.log.levels.ERROR)
    return
  end

  -- local put_method = put_to == 'above' and 'put!' or 'put'
  -- TODO: なんかうまくいかないので他の方法でやる
  -- vim.cmd(('%s="%s"'):format(put_method, vim.trim(current_filepath.stdout)))
  local put_method = put_to == 'above' and 'O' or 'o'
  vim.cmd(('normal! %s %s'):format(put_method, vim.trim(current_filepath.stdout)))
end

vim.keymap.set('n', '"gp', function()
  put_filepath_relative_to_git_root('bellow')
end)

vim.keymap.set('n', '"gP', function()
  put_filepath_relative_to_git_root('above')
end)

-- Operators and Objects
vim.keymap.set('n', 'ga', nvim.append_choose_surround_normal, { silent = true })
vim.keymap.set('n', 'gs', nvim.append_choose_surround_wide, { silent = true })
vim.keymap.set('n', 'ds', nvim.delete_mostly_inner_surround, { silent = true })
vim.keymap.set('n', 'cs', nvim.replace_mostly_inner_surround, { silent = true })
vim.keymap.set('n', 'dijp', 'v<Plug>(textobj-jabraces-parens-i)x', { remap = true })
vim.keymap.set('n', 'dajp', 'v<Plug>(textobj-jabraces-parens-a)x', { remap = true })
vim.keymap.set('n', 'dijK', 'v<Plug>(textobj-jabraces-yama-kakko-i)x', { remap = true })
vim.keymap.set('n', 'dajK', 'v<Plug>(textobj-jabraces-yama-kakko-a)x', { remap = true })
vim.keymap.set('n', 'dij-k', 'v<Plug>(textobj-jabraces-double-kakko-i)x', { remap = true })
vim.keymap.set('n', 'daj-k', 'v<Plug>(textobj-jabraces-double-kakko-a)x', { remap = true })
vim.keymap.set('n', '.', '<Plug>(repeat-.)', { remap = true })
vim.keymap.set('n', '<leader><leader>c', nvim.camelize_or_uncamelize_current_word_as_repeatable, { silent = true })
--- Fake operator
vim.keymap.set('n', '<C-v>ii', 'v<Plug>(textobj-indent-i)<C-v>ow', { remap = true }) -- Simular to vii, but select by <C-v>

--- Select inner line content (visual inner line)
vim.keymap.set('n', 'vil', function()
  vim.cmd('normal! ^vg_')
end, { silent = true })

-- Hit characters
vim.keymap.set('n', "'f", '<Plug>(fmap-forward-f)', { remap = true })
vim.keymap.set('n', "'F", '<Plug>(fmap-backward-f)', { remap = true })
vim.keymap.set('n', "'t", '<Plug>(fmap-forward-t)', { remap = true })
vim.keymap.set('n', "'T", '<Plug>(fmap-backward-T)', { remap = true })
vim.keymap.set('v', "'f", '<Plug>(fmap-forward-f)', { remap = true })
vim.keymap.set('v', "'F", '<Plug>(fmap-backward-f)', { remap = true })
vim.keymap.set('v', "'t", '<Plug>(fmap-forward-t)', { remap = true })
vim.keymap.set('v', "'T", '<Plug>(fmap-backward-T)', { remap = true })

-- ALE and LSP
vim.keymap.set('n', '<C-k><C-a>', '<Cmd>ALEToggle<CR>', { silent = true })
vim.keymap.set('n', '[]', nvim.open_diagnostic_detail)

vim.keymap.set('n', '[c', function()
  nvim.goto_diagnostic('previous')
end)

vim.keymap.set('n', ']c', function()
  nvim.goto_diagnostic('next')
end)

vim.keymap.set('n', 'K', function()
  vim.lsp.buf.hover({
    border = 'rounded',
    focusable = true,
    max_height = 20,
    max_width = 80,
  })
end)

vim.keymap.set('n', '<C-g><C-o>', function()
  vim.diagnostic.open_float(nil, {
    focusable = false,
    close_events = { 'BufLeave', 'CursorMoved', 'InsertEnter', 'FocusLost' },
    border = 'rounded',
  })
end, { silent = true })

vim.keymap.set('n', '<C-g><C-a>', vim.lsp.buf.code_action, { silent = true })
vim.keymap.set('n', '<C-g><C-d>', telescope.lsp_definitions, { silent = true })
vim.keymap.set('n', '<C-g>d', vim.lsp.buf.declaration, { silent = true })
vim.keymap.set('n', '<C-g><C-i>', vim.lsp.buf.implementation, { silent = true })
vim.keymap.set('n', '<C-g><C-t>', vim.lsp.buf.type_definition, { silent = true })

-- Programming Utils
vim.keymap.set('n', '<leader>R', '<Plug>(quickrepl-open)', { remap = true })
vim.keymap.set('n', '<leader>r', '<Cmd>Jaq<CR>', { nowait = true }) -- `nowait`: 僕の<leader>rを持ち去ってるのはだれですか。`:verbose nmap <leader>r*`してもこのキーマップしか出ない

-- File Manupilation
vim.keymap.set('n', '<leader>b', '<Cmd>MadoScratchOpenFile md<CR>', { silent = true })
vim.keymap.set('n', '<leader>B', '<Cmd>MadoScratchOpenFileNext md<CR>', { silent = true })
vim.keymap.set('n', '<leader><leader>b', ':<C-u>MadoScratchOpen', { remap = true })

vim.keymap.set('n', '<leader><leader>B', function()
  vim.cmd('vertical split ' .. InitLua.memo_path)
end, { silent = true })

-- File Editing
vim.keymap.set('n', '<C-k><Space>', nvim.remove_trailing_spaces, { silent = true })
vim.keymap.set('n', '<Space><Space>', nvim.compress_spaces, { silent = true })
vim.keymap.set('n', '<leader><leader>s', 'vii:sort<CR>', { remap = true, silent = true })

vim.keymap.set('n', '<C-k><C-s>', function()
  return s([[:%s/\m\C\<{word}\>//g<Left><Left>]], { word = vim.fn.expand('<cword>') })
end, { expr = true })

vim.keymap.set('n', '<C-k>s', function()
  return s([[:%s/\m\C\<{word}\>/{word}/g<Left><left>]], { word = vim.fn.expand('<cword>') })
end, { expr = true })

-- Git Operations
-- vim.keymap.set('n', '<leader>gs', '<Cmd>GinStatus<CR>', { silent = true }) -- TODO: なぜか（少なくともmacOSでは）cmdpaletteごしで開かないとNeovimが固まるので、一旦remap ↓ で対応する。暇な時に直す！
vim.keymap.set('n', '<leader>gs', ':<C-u>tabnew | GinStatus<CR>', { remap = true, silent = true }) -- remapなので、:でcmdpaletteが開く
vim.keymap.set('n', '"gs', ':<C-u>GinStatus<CR>', { remap = true, silent = true }) -- remapなので、:でcmdpaletteが開く
vim.keymap.set('n', '<leader>gl', '<Cmd>GitLog -100 --name-only<CR>', { silent = true }) -- Use my :GitLog due to :GinLog ignores arguments currently
vim.keymap.set('n', '<leader>gL', '<Cmd>GitLog -100 --patch<CR>', { silent = true })

vim.keymap.set('n', '<leader>go', function()
  git_log.open_buffer({ '-100', '--oneline', '--pretty=%h %ad %s', '--date=format:%Y-%m-%d %H:%M' })
end, { silent = true })

-- }}}
-- insert mode {{{

-- Fake digraphs
-- -> See below 'digraphs' section

-- Other than below
vim.keymap.set('i', '<C-j>', '<CR>')
vim.keymap.set('i', '<C-l>', '<Esc>')
vim.keymap.set('i', '<C-a>', '<Right>')
vim.keymap.set('i', '<C-k><C-k>', '<C-o>"_d$')
vim.keymap.set('i', '<C-k><C-j>', '<Esc>:write<CR>', { silent = true })
vim.keymap.set('i', '<C-k>J', '<Esc>:wall | echo "written all!"<CR>', { silent = true })
vim.keymap.set('i', '<C-b>', network.fetch_webpage_title, { silent = true, expr = true })

-- Copy and paste
vim.keymap.set('i', "<C-r>'", '<C-r>+')
vim.keymap.set('i', '<C-r>n', '<C-r>=expand("%:t")<CR>') -- Meaning "n"ame
vim.keymap.set('i', '<C-r>gr', '<C-r>=luaeval("InitLua.git_root")<CR>')
vim.keymap.set('i', '<C-r>gb', '<C-r>=system("git branch --show-current")<CR>')

-- Informations
vim.keymap.set('i', '<C-g><Tab>', [[copilot#Accept("\<CR>")]], {
  expr = true,
  replace_keycodes = false,
})
vim.keymap.set('i', '<C-g><C-n>', '<Plug>(copilot-next)', { remap = true })

-- File Editing
vim.keymap.set('i', '<C-s>', function()
  ---@diagnostic disable-next-line: undefined-field --なんで見つからないんだろう？ もしかしたらtypesディレクトリに追加した方がいい？
  if not snip.expand_or_jumpable() then
    vim.notify('No snippet to expand or jump to', vim.log.levels.INFO)
    return
  end
  ---@diagnostic disable-next-line: undefined-field
  snip.expand_or_jump()
end, { silent = true })

-- File Manupilation
vim.keymap.set('v', '<leader>B', '"zy<Cmd>MadoScratchOpenFileNext md<CR>V"zpgg=Ggg<Cmd>write<CR>', { silent = true })

-- }}}
-- select mode {{{

vim.keymap.set('s', '<C-l>', '<Esc>')

-- File Editing
vim.keymap.set('s', '<C-s>', function()
  ---@diagnostic disable-next-line: undefined-field --なんで見つからないんだろう？
  if not snip.jumpable(1) then
    vim.notify('No snippet to jump to', vim.log.levels.INFO)
    return
  end
  ---@diagnostic disable-next-line: undefined-field
  snip.jump(1)
end, { silent = true })

-- }}}
-- command-line mode {{{

vim.keymap.set('c', '<C-]>', [[\m\C\<\><Left><Left>]])
vim.keymap.set('c', '<C-b>', '<Left>')
vim.keymap.set('c', '<C-f>', '<Right>')
vim.keymap.set('c', '<C-a>', '<Home>')
vim.keymap.set('c', '<C-h>', '<BS>')
vim.keymap.set('c', '<C-d>', '<Del>')
vim.keymap.set('c', '<C-e>', '<End>')
vim.keymap.set('c', '<C-k><C-k>', nvim.remove_text_after_cursor, { expr = true })
vim.keymap.set('c', '<C-l>', '<C-c>')
vim.keymap.set('c', '<C-o>', '<Up>')
vim.keymap.set('c', '<C-y>', '<Down>')
vim.keymap.set('c', "<C-r>'", '<C-r>+')
vim.keymap.set('c', '<C-r>n', '<C-r>=expand("%:t")<CR>') -- Meaning "n"ame
vim.keymap.set('c', '<C-r>g', '<C-r>=luaeval("InitLua.git_root")<CR>')

-- }}}
-- visual/operator mode {{{

-- Other than below
vim.keymap.set('v', '<C-l>', '<Esc>')
vim.keymap.set('v', 'g_', '$')
vim.keymap.set('v', '<leader>p', '"+p')
vim.keymap.set('v', '<leader>P', '"+P')
vim.keymap.set('v', '<leader>y', '"+y')
vim.keymap.set('v', '<leader>d', '"+d')
vim.keymap.set('v', '<leader>x', '"+x')
vim.keymap.set('v', '<leader>w', '<Plug>(openbrowser-open)', { remap = true })

-- Folds
vim.keymap.set('v', 'zo', 'zogv')
vim.keymap.set('v', 'zO', 'zOgv')

-- Don't select blanks
vim.keymap.set('v', 'a"', '2i"')
vim.keymap.set('o', 'a"', '2i"')
vim.keymap.set('v', "a'", "2i'")
vim.keymap.set('o', "a'", "2i'")
vim.keymap.set('v', 'a`', '2i`')
vim.keymap.set('o', 'a`', '2i`')

-- Quotes
vim.keymap.set('v', 'ab', '2i`')
vim.keymap.set('o', 'ab', '2i`')
vim.keymap.set('v', 'ib', 'i`')
vim.keymap.set('o', 'ib', 'i`')

-- Brackets
vim.keymap.set('v', 'ap', 'a(')
vim.keymap.set('o', 'ap', 'a(')
vim.keymap.set('v', 'aP', 'a{')
vim.keymap.set('o', 'aP', 'a{')
vim.keymap.set('v', 'ak', 'a[')
vim.keymap.set('o', 'ak', 'a[')
vim.keymap.set('v', 'aK', 'a<')
vim.keymap.set('o', 'aK', 'a<')
vim.keymap.set('v', 'ip', 'i(')
vim.keymap.set('o', 'ip', 'i(')
vim.keymap.set('v', 'iP', 'i{')
vim.keymap.set('o', 'iP', 'i{')
vim.keymap.set('v', 'ik', 'i[')
vim.keymap.set('o', 'ik', 'i[')
vim.keymap.set('v', 'iK', 'i<')
vim.keymap.set('o', 'iK', 'i<')

-- Operators and Objects
vim.keymap.set('v', 'ga', '<Plug>(operator-surround-append)')
vim.keymap.set('o', 'ga', '<Plug>(operator-surround-append)')
vim.keymap.set('v', 'ai', '<Plug>(textobj-indent-a)')
vim.keymap.set('v', 'ii', '<Plug>(textobj-indent-i)')
vim.keymap.set('v', 'ijp', '<Plug>(textobj-jabraces-parens-i)')
vim.keymap.set('v', 'ajp', '<Plug>(textobj-jabraces-parens-a)')
vim.keymap.set('v', 'ijK', '<Plug>(textobj-jabraces-yama-kakko-i)')
vim.keymap.set('v', 'ajK', '<Plug>(textobj-jabraces-yama-kakko-a)')
vim.keymap.set('v', 'ij-k', '<Plug>(textobj-jabraces-double-kakko-i)')
vim.keymap.set('v', 'aj-k', '<Plug>(textobj-jabraces-double-kakko-a)')
vim.keymap.set('v', 'i_', '<Plug>(textobj-between-i)_') -- TODO: 動いてない
vim.keymap.set('v', 'a_', '<Plug>(textobj-between-a)_') --
vim.keymap.set('v', 'iB', '<Plug>(textobj-between-i)*') -- TODO: **を対象にしたい（vim-textobj-betweenは多分1-charのみ対応）
vim.keymap.set('v', 'aB', '<Plug>(textobj-between-a)*')
-- TODO: 今度↑をvim-surround-operatorで置き換える（可能なら）
-- --- vim-operator-surround
-- vim.keymap.set('v', 'ajp', '<Plug>(operator-surround-append)gajp')
-- vim.keymap.set('o', 'ajp', '<Plug>(operator-surround-append)gajp')
--           { block = { '「', '」' }, motionwise = { 'char', 'line', 'block' }, keys = { 'jk' } },
-- vim.keymap.set('v', '', '<Plug>(operator-surround-append)gajp')
-- vim.keymap.set('o', 'ajp', '<Plug>(operator-surround-append)gajp')
--           { block = { '【', '】' }, motionwise = { 'char', 'line', 'block' }, keys = { 'jK' } },
-- vim.keymap.set('v', 'ajp', '<Plug>(operator-surround-append)gajp')
-- vim.keymap.set('o', 'ajp', '<Plug>(operator-surround-append)gajp')
--           { block = { '『', '』' }, motionwise = { 'char', 'line', 'block' }, keys = { 'j-k' } },
-- vim.keymap.set('v', 'ajp', '<Plug>(operator-surround-append)gajp')
-- vim.keymap.set('o', 'ajp', '<Plug>(operator-surround-append)gajp')

vim.keymap.set('v', 'a_', function()
  return vim.fn['textobj#from_regexp#mapexpr']('[^A-Za-z0-9][A-Za-z0-9]\\+[^A-Za-z0-9]')
end, { expr = true })

vim.keymap.set('v', 'i_', function()
  return vim.fn['textobj#from_regexp#mapexpr']('[A-Za-z0-9]\\+')
end, { expr = true })

vim.keymap.set('v', 'al', function()
  return vim.fn['textobj#from_regexp#mapexpr']('^.*$')
end, { expr = true })

vim.keymap.set('v', 'il', function()
  return vim.fn['textobj#from_regexp#mapexpr']('^\\s*\\zs.*\\ze.*$')
end, { expr = true })

vim.keymap.set('o', 'il', function()
  return vim.fn['textobj#from_regexp#mapexpr']('^\\s*\\zs.*\\ze.*$')
end, { expr = true })

-- Programming Utils
vim.keymap.set('v', '<leader>r', '<Plug>(neoquickrun)', { remap = true })

-- File Editing
vim.keymap.set('v', '<leader><leader>s', ':sort<CR>')
vim.keymap.set('v', '<leader><leader>c', '<Plug>(operator-camelize-toggle)', { remap = true })

-- }}}
-- terminal mode {{{

vim.keymap.set('t', '<C-l>', [[<C-\><C-n>]])
vim.keymap.set('t', [[<C-\><C-n>]], '<Esc>')
vim.keymap.set('t', '<C-[>', '<Esc>')
vim.keymap.set('t', '<C-]>', '<C-l>')

-- }}}
-- digraphs {{{

vim.cmd([[
digraph (( 8834   " ⊂ right includes left
digraph )) 8835   " ⊃ left includes right
digraph /= 8800   " ≠ not equal
digraph \* 215    " × cartesian product
digraph xx 215    " × cartesian product
digraph \. 9675   " ○ composite
digraph \/ 247    " ÷ division
digraph \< 8804   " ≤ right more than left or equals
digraph \= 8803   " ＝ equivalence relation
digraph \> 8805   " ≥ left mode than right or equals
digraph \A 8704   " ∀ forall
digraph \E 8707   " ∃ exists
digraph \U 8745   " ∩ intersect
digraph \u 8746   " ∪ union
digraph \a 8743   " ∧ and
digraph \o 8744   " ∨ or
digraph \|^ 8593  " ↑ arrow up
digraph \|v 8595  " ↓ arrow down
digraph up 8593   " ↑ arrow up
digraph dn 8595   " ↓ arrow down
digraph ph 934    " Φ phi
digraph pi 960    " π pi
]])

-- fake digraphs
vim.keymap.set('i', '<C-k>\\+', '＋')
vim.keymap.set('i', '<C-k>\\-', '−')
vim.keymap.set('i', '<C-k>\\=', '＝')
vim.keymap.set('i', '<C-k>?=', '≒')
vim.keymap.set('i', '<C-k>=~', '≅')
vim.keymap.set('i', '<C-k>\\N', 'ℕ')
vim.keymap.set('i', '<C-k>\\Z', 'ℤ')
vim.keymap.set('i', '<C-k>\\R', 'ℝ')
vim.keymap.set('i', '<C-k>\\Q', 'ℚ')
vim.keymap.set('i', '<C-k>\\C', 'ℂ')
vim.keymap.set('i', '<C-k>..', '◉')
vim.keymap.set('i', '<C-k>\\|>', '↦')

-- }}}
-- operator-pending mode {{{

-- }}}
-- abbr {{{

vim.cmd([[
inoreabbr reuslt result
inoreabbr unkonwn unknown
inoreabbr uknown unknown
inoreabbr Parnes Parens
inoreabbr parnes parens
inoreabbr reuslt result
inoreabbr Encrpyt Encrypt
inoreabbr encrpyt encrypt
]])

-- }}}
