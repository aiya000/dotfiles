---keymaps.lua のサブセット。lightweight版から使われるキーマップのみ定義する。

local fn = require('utils.functions')
local nvim = require('nvim-lightweight')
local network = require('utils.network')
local telescope = require('telescope.builtin')
local snip = require('luasnip')

local s = fn.s

-- normal mode: 基本操作 {{{

vim.keymap.set('n', '<C-c>', '<NOP>')
vim.keymap.set('n', '<C-c><C-c>', '<C-c>')

vim.keymap.set('n', '<C-g>', '<NOP>')
vim.keymap.set('n', '<C-g><C-g>', '<C-g>')

vim.keymap.set('n', '<CR>', 'o<Esc>')
nvim.keymaps_set('n', { '<C-j>', '<C-m>' }, '<CR>', { remap = true })
nvim.keymaps_set('n', { '<C-[>', '<Esc>', '<C-l>' }, nvim.clear, { silent = true })
vim.keymap.set('n', '<C-k><C-l>', nvim.clear_highlight_deeply)
vim.keymap.set('n', '<C-k>o', '<Cmd>e! %<CR>', { silent = true })
vim.keymap.set('n', 'gG', 'ggVG')
vim.keymap.set('n', "'gG", 'ggVG"+y')
vim.keymap.set('n', 'q:', ':')
vim.keymap.set('n', '(', '(zv')
vim.keymap.set('n', ')', ')zv')
vim.keymap.set('n', '<C-k><C-j>', nvim.clear_highlight_and_write)
vim.keymap.set('n', '<C-k>J', '<Cmd>wall | echo "written all !"<CR>', { silent = true })
vim.keymap.set('n', '<C-]>', 'g<C-]>')
vim.keymap.set('n', '<leader>q', '<Cmd>copen<CR><C-w>H', { silent = true })
vim.keymap.set('n', '<leader><leader>q', '<Cmd>cclose<CR>', { silent = true })
vim.keymap.set('n', 'Y', 'yg_')
vim.keymap.set('n', 'g<C-]>', '<C-]>')
vim.keymap.set('n', 'g_', '$')
vim.keymap.set('n', 'zs', 'zszh')
vim.keymap.set('n', '{', '{zv')
vim.keymap.set('n', '}', '}zv')
vim.keymap.set('n', ',,', 'ggVG"+y<C-o>')
vim.keymap.set('n', ',<', 'ggVG"_s')
vim.keymap.set('n', '<C-x><C-n>', '<C-n>')
vim.keymap.set('n', '<C-x><C-p>', '<C-p>')
vim.keymap.set('n', '/', [[/\m]])
vim.keymap.set('n', '?', [[?\m]])

vim.keymap.set('n', 'gJ', function()
  local next_line = vim.fn.getline(vim.fn.line('.') + 1)
  vim.fn.setline(vim.fn.line('.') + 1, (next_line:gsub('^%s+', '')))
  vim.cmd('normal! gJ')
end)

vim.keymap.set('n', 'gF', function()
  local file = vim.fn.expand('<cfile>')
  if vim.fn.filereadable(file) ~= 1 then
    vim.notify('Force opened: ' .. file, vim.log.levels.INFO)
  end
  vim.cmd.edit(file)
end)

vim.keymap.set('n', 'cijp', 'vijps', { remap = true })
vim.keymap.set('n', 'cajp', 'vajps', { remap = true })

-- }}}
-- normal mode: 検索 {{{

local function try_show_search_number_or_do_nothing()
  local ok, hlslens = pcall(require, 'hlslens')
  if not ok then return end
  hlslens.start()
end

vim.keymap.set('n', 'g*', function()
  local pos = vim.fn.getpos('.')
  vim.cmd('silent! normal! *')
  vim.fn.setpos('.', pos)
  try_show_search_number_or_do_nothing()
end)

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

-- }}}
-- normal mode: Q（バッファ閉じ系）{{{

vim.keymap.set('n', 'Q', function()
  if nvim.close_quickfix_if_open() then return end
  local closing_target_buffer_filetype = {
    'diff', 'gin-branch', 'gin-log', 'gin-status', 'git-log', 'git-show', 'netrw', 'quickrun',
  }
  nvim.bufclose_filetype(closing_target_buffer_filetype)
end)

-- }}}
-- normal mode: fold {{{

vim.keymap.set('n', 'h', function()
  return vim.fn.foldclosed('.') > -1 and 'zo' or 'h'
end, { expr = true })

vim.keymap.set('n', 'l', function()
  return vim.fn.foldclosed('.') > -1 and 'zo' or 'l'
end, { expr = true })

vim.keymap.set('n', 'zj', 'zjzo')
vim.keymap.set('n', 'zk', 'zkzo[zzt')
vim.keymap.set('n', 'zC', 'zM')
vim.keymap.set('n', 'zA', 'zR')

-- }}}
-- normal mode: ウィンドウ・バッファ・タブ {{{

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

vim.keymap.set('n', '<C-s>N', function() nvim.move_window_forward() end, { silent = true })
vim.keymap.set('n', '<C-s>P', function() nvim.move_window_forward() end, { silent = true })

vim.keymap.set('n', '<C-n>', nvim.tabnext_loop, { silent = true })
vim.keymap.set('n', '<C-p>', nvim.tabprev_loop, { silent = true })

vim.keymap.set('n', '<C-s>n', function()
  nvim.move_tab_next()
  if InitLua.hydra and InitLua.hydra.tab_move then
    InitLua.hydra.tab_move:activate()
  end
end)

vim.keymap.set('n', '<C-s>p', function()
  nvim.move_tab_prev()
  if InitLua.hydra and InitLua.hydra.tab_move then
    InitLua.hydra.tab_move:activate()
  end
end)

-- }}}
-- normal mode: ターミナル {{{

local function fallback_to_path_at_started()
  vim.api.nvim_echo({ { 'No directory found. Fallback to the path at started: ' .. InitLua.path_at_started } }, false, {})
  return InitLua.path_at_started
end

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

vim.keymap.set('n', '<leader>V', function()
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

vim.keymap.set('n', '"V', nvim.toggle_shell)

-- }}}
-- normal mode: ファイルエクスプローラ {{{

vim.keymap.set('n', '<leader>e', '<Cmd>vsp|Oil<CR>', { silent = true })
vim.keymap.set('n', '<leader>E', '<Cmd>Oil<CR>', { silent = true })
vim.keymap.set('n', '<leader><leader>e', '<Cmd>sp|Oil<CR>', { silent = true })
vim.keymap.set('n', '<leader><leader>E', '<Cmd>tabnew|Oil<CR>', { silent = true })

-- }}}
-- normal mode: Telescope {{{

vim.keymap.set('n', '<C-k><C-e>', function()
  telescope.find_files({ hidden = true, no_ignore = true, no_ignore_parent = true })
end)

vim.keymap.set('n', '<C-k><C-f>', function()
  telescope.lsp_document_symbols({ symbols = { 'function' } })
end)

vim.keymap.set('n', '<C-k>f', telescope.treesitter)
vim.keymap.set('n', '<C-k><C-r>', telescope.reloader)
vim.keymap.set('n', 'L', telescope.buffers)
vim.keymap.set('n', 'H', telescope.live_grep)
vim.keymap.set('n', 'M', telescope.oldfiles)
vim.keymap.set('n', 'm>', telescope.marks)
vim.keymap.set('n', 'q>', telescope.registers)
vim.keymap.set('n', 'y>', '<Cmd>Telescope yank_history<CR>', { silent = true })

vim.keymap.set('n', 'g>', function()
  local buf = vim.api.nvim_create_buf(false, true)
  local output = vim.fn.execute('messages')
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, vim.split(output, '\n'))
  nvim.open_buffer_in_float_window(buf)
end, { silent = true })

-- }}}
-- normal mode: AI Agent {{{

vim.keymap.set('n', '<leader>gc', nvim.toggle_copilot_cli)
vim.keymap.set('n', '<leader>ac', nvim.toggle_antigravity_cli)
vim.keymap.set('n', '<leader>dc', nvim.toggle_devin_cli)

-- }}}
-- normal mode: オプショントグル {{{

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

-- }}}
-- normal mode: コピペ {{{

vim.keymap.set('n', 'gp', function()
  local reg = string.sub(vim.fn.getregtype(), 1, 1)
  return ('`[%s`]'):format(reg)
end, { expr = true })

vim.keymap.set('n', '<leader>p', '"+p')
vim.keymap.set('n', '<leader>P', '"+P')
vim.keymap.set('n', '<leader>y', '"+y')
vim.keymap.set('n', '<leader>Y', '"+yg_')
vim.keymap.set('n', '<leader>dd', '"+dd')
vim.keymap.set('n', '<leader>D', '"+D')
vim.keymap.set('n', '<leader>d', '"+d')
vim.keymap.set('n', '<leader>x', '"+x')

vim.keymap.set('n', '"gp', function()
  local result = vim.system({ 'git', 'ls-files', '--full-name', vim.fn.expand('%') }):wait()
  if result.code ~= 0 then vim.notify(result.stderr, vim.log.levels.ERROR); return end
  vim.cmd(('normal! o %s'):format(vim.trim(result.stdout)))
end)

vim.keymap.set('n', '"gP', function()
  local result = vim.system({ 'git', 'ls-files', '--full-name', vim.fn.expand('%') }):wait()
  if result.code ~= 0 then vim.notify(result.stderr, vim.log.levels.ERROR); return end
  vim.cmd(('normal! O %s'):format(vim.trim(result.stdout)))
end)

-- }}}
-- normal mode: オペレータ・テキストオブジェクト {{{

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
vim.keymap.set('n', '<C-v>ii', 'v<Plug>(textobj-indent-i)<C-v>ow', { remap = true })

vim.keymap.set('n', 'vil', function()
  vim.cmd('normal! ^vg_')
end, { silent = true })

-- }}}
-- normal mode: f/t拡張 {{{

vim.keymap.set('n', "'f", '<Plug>(fmap-forward-f)', { remap = true })
vim.keymap.set('n', "'F", '<Plug>(fmap-backward-f)', { remap = true })
vim.keymap.set('n', "'t", '<Plug>(fmap-forward-t)', { remap = true })
vim.keymap.set('n', "'T", '<Plug>(fmap-backward-T)', { remap = true })
vim.keymap.set('v', "'f", '<Plug>(fmap-forward-f)', { remap = true })
vim.keymap.set('v', "'F", '<Plug>(fmap-backward-f)', { remap = true })
vim.keymap.set('v', "'t", '<Plug>(fmap-forward-t)', { remap = true })
vim.keymap.set('v', "'T", '<Plug>(fmap-backward-T)', { remap = true })

-- }}}
-- normal mode: オプショントグル（ファイル操作） {{{

vim.keymap.set('n', '<C-k><Space>', nvim.remove_trailing_spaces, { silent = true })
vim.keymap.set('n', '<Space><Space>', nvim.compress_spaces, { silent = true })
vim.keymap.set('n', '<leader><leader>s', 'vii:sort<CR>', { remap = true, silent = true })

vim.keymap.set('n', '<C-k><C-s>', function()
  return s([[:%s/\m\C\<{word}\>//g<Left><Left>]], { word = vim.fn.expand('<cword>') })
end, { expr = true })

vim.keymap.set('n', '<C-k>s', function()
  return s([[:%s/\m\C\<{word}\>/{word}/g<Left><left>]], { word = vim.fn.expand('<cword>') })
end, { expr = true })

-- }}}
-- normal mode: ファイル操作 {{{

vim.keymap.set('n', '<leader><leader>B', function()
  vim.cmd('vertical split ' .. InitLua.memo_path)
end, { silent = true })

-- }}}
-- insert mode {{{

vim.keymap.set('i', '<C-j>', '<CR>')
vim.keymap.set('i', '<C-l>', '<Esc>')
vim.keymap.set('i', '<C-a>', '<Right>')
vim.keymap.set('i', '<C-k><C-k>', '<C-o>"_d$')
vim.keymap.set('i', '<C-k><C-j>', '<Esc>:write<CR>', { silent = true })
vim.keymap.set('i', '<C-k>J', '<Esc>:wall | echo "written all!"<CR>', { silent = true })
vim.keymap.set('i', '<C-b>', network.fetch_webpage_title, { silent = true, expr = true })

vim.keymap.set('i', "<C-r>'", '<C-r>+')
vim.keymap.set('i', '<C-r>n', '<C-r>=expand("%:t")<CR>')
vim.keymap.set('i', '<C-r>gr', '<C-r>=luaeval("InitLua.git_root")<CR>')
vim.keymap.set('i', '<C-r>gb', '<C-r>=system("git branch --show-current")<CR>')

vim.keymap.set('i', '<C-g><Tab>', [[copilot#Accept("\<CR>")]], { expr = true, replace_keycodes = false })
vim.keymap.set('i', '<C-g><C-n>', '<Plug>(copilot-next)', { remap = true })

vim.keymap.set('i', '<C-s>', function()
  ---@diagnostic disable-next-line: undefined-field
  if not snip.expand_or_jumpable() then
    vim.notify('No snippet to expand or jump to', vim.log.levels.INFO)
    return
  end
  ---@diagnostic disable-next-line: undefined-field
  snip.expand_or_jump()
end, { silent = true })

-- }}}
-- select mode {{{

vim.keymap.set('s', '<C-l>', '<Esc>')

vim.keymap.set('s', '<C-s>', function()
  ---@diagnostic disable-next-line: undefined-field
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
vim.keymap.set('c', '<C-r>n', '<C-r>=expand("%:t")<CR>')
vim.keymap.set('c', '<C-r>g', '<C-r>=luaeval("InitLua.git_root")<CR>')

-- }}}
-- visual/operator mode {{{

vim.keymap.set('v', '<C-l>', '<Esc>')
vim.keymap.set('v', 'g_', '$')
vim.keymap.set('v', '<leader>p', '"+p')
vim.keymap.set('v', '<leader>P', '"+P')
vim.keymap.set('v', '<leader>y', '"+y')
vim.keymap.set('v', '<leader>d', '"+d')
vim.keymap.set('v', '<leader>x', '"+x')
vim.keymap.set('v', '<leader>w', '<Plug>(openbrowser-open)', { remap = true })

vim.keymap.set('v', 'zo', 'zogv')
vim.keymap.set('v', 'zO', 'zOgv')

vim.keymap.set('v', 'a"', '2i"')
vim.keymap.set('o', 'a"', '2i"')
vim.keymap.set('v', "a'", "2i'")
vim.keymap.set('o', "a'", "2i'")
vim.keymap.set('v', 'a`', '2i`')
vim.keymap.set('o', 'a`', '2i`')

vim.keymap.set('v', 'ab', '2i`')
vim.keymap.set('o', 'ab', '2i`')
vim.keymap.set('v', 'ib', 'i`')
vim.keymap.set('o', 'ib', 'i`')

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
vim.keymap.set('v', 'i_', '<Plug>(textobj-between-i)_')
vim.keymap.set('v', 'a_', '<Plug>(textobj-between-a)_')
vim.keymap.set('v', 'iB', '<Plug>(textobj-between-i)*')
vim.keymap.set('v', 'aB', '<Plug>(textobj-between-a)*')

vim.keymap.set({ 'v', 'o' }, 'a_', function()
  return vim.fn['textobj#from_regexp#mapexpr']('[^A-Za-z0-9][A-Za-z0-9]\\+[^A-Za-z0-9]')
end, { expr = true })

vim.keymap.set({ 'v', 'o' }, 'i_', function()
  return vim.fn['textobj#from_regexp#mapexpr']('[A-Za-z0-9]\\+')
end, { expr = true })

vim.keymap.set({ 'v', 'o' }, 'il', function()
  return vim.fn['textobj#from_regexp#mapexpr']('^\\s*\\zs.*\\ze.*$')
end, { expr = true })

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
digraph (( 8834
digraph )) 8835
digraph /= 8800
digraph \* 215
digraph xx 215
digraph \. 9675
digraph \/ 247
digraph \< 8804
digraph \= 8803
digraph \> 8805
digraph \A 8704
digraph \E 8707
digraph \U 8745
digraph \u 8746
digraph \a 8743
digraph \o 8744
digraph \|^ 8593
digraph \|v 8595
digraph up 8593
digraph dn 8595
digraph ph 934
digraph pi 960
]])

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
vim.keymap.set('i', '<C-k>(=', '⊆')
vim.keymap.set('i', '<C-k>=)', '⊇')
vim.keymap.set('i', '<C-k>!(=', '⊊')
vim.keymap.set('i', '<C-k>!=)', '⊋')
vim.keymap.set('i', '<C-k>\\|>', '↦')
vim.keymap.set('i', '<C-k>..', '◉')
vim.keymap.set('i', '<C-k>oo', '⚪︎')
vim.keymap.set('i', '<C-k>OO', '⭕')
vim.keymap.set('i', '<C-k>xx', '×')
vim.keymap.set('i', '<C-k>XX', '❌ ')
vim.keymap.set('i', '<C-k>tt', '△')
vim.keymap.set('i', '<C-k>kk', '◻︎')

-- }}}
-- abbr {{{

vim.cmd([[
inoreabbr reuslt result
inoreabbr unkonwn unknown
inoreabbr uknown unknown
inoreabbr Parnes Parens
inoreabbr parnes parens
inoreabbr Encrpyt Encrypt
inoreabbr encrpyt encrypt
]])

-- }}}

-- vim: foldmethod=marker
