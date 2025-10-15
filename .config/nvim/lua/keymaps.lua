local fn = require('utils.functions')
local git_log = require('git-log')
local helper = require('helper')
local network = require('utils.network')
local telescope = require('telescope.builtin')
local snip = require('luasnip')

local s = fn.s

local function map(mode, lhs, rhs, opts)
  opts = opts or {}
  vim.keymap.set(mode, lhs, rhs, opts)
end

-- normal mode {{{

-- Allow keymaps like <C-c>{foo}, and {bar}<C-c>
map('n', '<C-c>', '<NOP>')
map('n', '<C-c><C-c>', '<C-c>')

-- Allow keymaps like <C-g>{foo}, and {bar}<C-g>
map('n', '<C-g>', '<NOP>')
map('n', '<C-g><C-g>', '<C-g>')

-- flash.nvimでfFtTでの進む/戻るを手に入れたので、矯正ギプスを試しに使ってみる
map('n', ',', '<NOP>')
map('n', ';', '<NOP>')

-- Other than below
map('n', '<CR>', 'o<Esc>')
helper.keymaps_set('n', { '<C-j>', '<C-m>' }, '<CR>', { remap = true })
helper.keymaps_set('n', { '<C-[>', '<Esc>', '<C-l>' }, helper.clear, { silent = true })
map('n', '<C-k><C-l>', helper.clear_highlight_deeply)
map('n', '<C-k>o', '<Cmd>e! %<CR>', { silent = true })
map('n', '-', '-') -- デフォルト（？）で、なぜかdirvishが開くので、無効化
map('n', 'gG', 'ggVG')
map('n', 'q:', ':') -- `:`でcmdpaletteを開くので逆に`q:`でcmd-modeを開く
map('n', '(', '(zv')
map('n', ')', ')zv')
map('n', '<C-k><C-j>', helper.clear_highlight_and_write)
map('n', '<C-k>J', '<Cmd>wall | echo "written all !"<CR>', { silent = true })
map('n', '<C-]>', 'g<C-]>')
map('n', '<leader>q', '<Cmd>copen<CR>', { silent = true })
map('n', '<leader><leader>q', '<Cmd>cclose<CR>', { silent = true })
map('n', 'Y', 'yg_')
map('n', 'g<C-]>', '<C-]>')
map('n', 'g_', '$')
map('n', 'zs', 'zszh')
map('n', '{', '{zv')
map('n', '}', '}zv')
map('n', '<C-x><C-n>', '<C-n>')
map('n', '<C-x><C-p>', '<C-p>')
map('n', '<leader>w', '<Plug>(openbrowser-open)', { remap = true })
map('n', '<leader>U', '<Cmd>UndotreeToggle<CR>', { silent = true })

-- Search
local function try_show_search_number_or_do_nothing()
  local ok, hlslens = pcall(require, 'hlslens')
  if not ok then
    return
  end
  hlslens.start()
end

--- Search cword without moving
map('n', 'g*', function()
  local pos = vim.fn.getpos('.')
  vim.cmd('silent! normal! *')
  vim.fn.setpos('.', pos)
  try_show_search_number_or_do_nothing()
end)

--- Keep search direction
map('n', 'n', function()
  vim.cmd('silent! normal! ' .. (vim.v.searchforward == 1 and 'nzv' or 'Nzv'))
  try_show_search_number_or_do_nothing()
end)

map('n', 'N', function()
  vim.cmd('silent! normal! ' .. (vim.v.searchforward == 1 and 'Nzv' or 'nzv'))
  try_show_search_number_or_do_nothing()
end)

map('n', '*', function()
  vim.cmd('silent! normal! *zv')
  try_show_search_number_or_do_nothing()
end)

map('n', '#', function()
  vim.cmd('silent! normal! #zv')
  try_show_search_number_or_do_nothing()
end)

-- Close windows of temporary buffers
local temporary_buftypes = {
  'aref_web',
  'diff',
  'gin-branch',
  'gin-log',
  'gin-status',
  'git-log',
  'git-show',
  'help',
  'man',
  'netrw',
  'dirvish',
  'quickrun',
  'scratch',
  'ddu-ff',
  'ddu-filter',
  'fern',
  'translate', -- translate.nvim
}
map('n', 'Q', function()
  helper.bufclose_filetype(temporary_buftypes)
end, { silent = true })

-- Foldings
map('n', 'h', function()
  return vim.fn.foldclosed('.') > -1 and 'zo' or 'h'
end, { expr = true })

map('n', 'l', function()
  return vim.fn.foldclosed('.') > -1 and 'zo' or 'l'
end, { expr = true })

map('n', 'zj', 'zjzo')
map('n', 'zk', 'zkzo[zzt')

-- Windows, buffers, and tabs
map('n', '<Space>h', '<C-w>h')
map('n', '<Space>j', '<C-w>j')
map('n', '<Space>k', '<C-w>k')
map('n', '<Space>l', '<C-w>l')

map('n', 'ghR', '<C-w>r')
map('n', 'ghq', '<Cmd>q<CR>', { silent = true })
map('n', 'ghQ', '<Cmd>quitall<CR>', { silent = true })
map('n', 'ghc', '<Cmd>bdelete<CR>', { silent = true })
map('n', 'ghC', '<Cmd>bdelete!<CR>', { silent = true })
map('n', 'gho', '<Cmd>only<CR>', { silent = true })
map('n', 'gh_', '<Cmd>resize<CR>', { silent = true })
map('n', 'gh"', '<Cmd>resize 5<CR>', { silent = true })
map('n', "gh'", '<Cmd>resize 10<CR>', { silent = true })
map('n', 'gh|', '<C-w>|')
map('n', 'gh\\', '<Cmd>vertical resize 1<CR>', { silent = true })
map('n', 'gh%', '<Cmd>vertical resize 20<CR>', { silent = true })
map('n', 'gh=', '<C-w>=')
map('n', 'gh+', 'gh_gh|', { remap = true })
map('n', 'ghH', '<C-w>H')
map('n', 'ghJ', '<C-w>J')
map('n', 'ghK', '<C-w>K')
map('n', 'ghL', '<C-w>L')
map('n', 'ghs', '<Cmd>split<CR>', { silent = true })
map('n', 'ghv', '<Cmd>vsplit<CR>', { silent = true })

map('n', 'gH', 'mZ:tabnew<CR>`Z', { silent = true })
map('n', 'ghh', 'mZ:hide<CR>:tabnew<CR>`Z', { silent = true })

map('n', '<C-w>q', '<NOP>')
map('n', '<C-w>c', '<NOP>')
map('n', '<C-w>r', '<NOP>')
map('n', '<C-w>_', '<NOP>')
map('n', '<C-w>\\', '<NOP>')
map('n', '<C-w>=', '<NOP>')
map('n', '<C-w>o', '<NOP>')
map('n', '<C-w>H', '<NOP>')
map('n', '<C-w>J', '<NOP>')
map('n', '<C-w>K', '<NOP>')
map('n', '<C-w>L', '<NOP>')
map('n', '<C-w>s', '<NOP>')
map('n', '<C-w>v', '<NOP>')
map('n', 'gh', '<NOP>')

--- Window navigations
map('n', '<C-s>N', function()
  helper.move_window_forward()
end, { silent = true })

map('n', '<C-s>P', function()
  helper.move_window_forward()
end, { silent = true })

--- Tabs navigations
map('n', '<C-n>', helper.tabnext_loop, { silent = true })
map('n', '<C-p>', helper.tabprev_loop, { silent = true })

--- Start tab move mode with moving the current tab
vim.keymap.set('n', '<C-s>n', function()
  helper.move_tab_next()
  if InitLua.hydra.tab_move == nil then
    vim.notify('InitLua.hydra.tab_move is not loaded', vim.log.levels.ERROR)
  else
    InitLua.hydra.tab_move:activate()
  end
end)

vim.keymap.set('n', '<C-s>p', function()
  helper.move_tab_prev()
  if InitLua.hydra.tab_move == nil then
    vim.notify('InitLua.hydra.tab_move is not loaded', vim.log.levels.ERROR)
  else
    InitLua.hydra.tab_move:activate()
  end
end)

-- `termopen()`
map('n', '<leader>v', function()
  local cwd = helper.read_current_buffer_dir() or error('No directory found')
  vim.cmd('vertical new')
  helper.termopen_shell({ cwd = cwd })
end)

map('n', '<leader><leader>v', function()
  local cwd = helper.read_current_buffer_dir() or error('No directory found')
  vim.cmd('new')
  helper.termopen_shell({ cwd = cwd })
end)

map('n', '<leader>V', function()
  -- Open in the current window
  -- NOTE: 現在のウィンドウでlspのエラーなどがあると、タイミングによってその表示を持ち越してしまうので、新しいウィンドウで開く
  local cwd = helper.read_current_buffer_dir() or error('No directory found')
  local current_win = vim.api.nvim_get_current_win()
  vim.cmd('new')
  helper.termopen_shell({ cwd = cwd }, false)
  vim.api.nvim_win_close(current_win, false)
  vim.fn.feedkeys('i')
end)

map('n', '<leader><leader>V', function()
  local cwd = helper.read_current_buffer_dir() or error('No directory found')
  vim.cmd('tabnew')
  helper.termopen_shell({ cwd = cwd })
end)

-- File explorer
map('n', '<leader>e', function()
  helper.toggle_explorer()
end, { silent = true })

map('n', '<leader><leader>e', function()
  helper.open_explorer('split')
end, { silent = true })

map('n', '<leader>E', function()
  helper.open_explorer('stay')
end, { silent = true })

map('n', '<leader><leader>E', function()
  helper.open_explorer('tabnew')
end, { silent = true })

map('n', [[\e]], function()
  helper.toggle_explorer(InitLua.path_at_started)
end, { silent = true })

map('n', [[\\e]], function()
  helper.open_explorer('split', InitLua.path_at_started)
end, { silent = true })

map('n', [[\E]], function()
  helper.open_explorer('stay', InitLua.path_at_started)
end, { silent = true })

map('n', [[\\E]], function()
  helper.open_explorer('tabnew', InitLua.path_at_started)
end, { silent = true })

-- List up
map('n', '<C-k><C-e>', function()
  telescope.find_files({ hidden = true })
end) -- TODO: もしパフォーマンスが遅ければ、このキーマッピングはカレントディレクトリ以下のみを表示して、プロジェクトルート以下の表示（`InitLua.git_root or InitLua.path_at_started`）は以下の<C-k>eに分担させる
-- map('n', '<C-k>e', telescope.find_files)

map('n', '<C-k><C-f>', ':<C-u>Telescope treesitter<CR>function')
map('n', '<C-k><C-r>', telescope.reloader)
map('n', 'L', telescope.buffers)
map('n', 'H', telescope.live_grep)
map('n', 'M', telescope.oldfiles)
map('n', 'm>', telescope.marks)
map('n', 'q>', telescope.registers)
map('n', 'y>', '<Cmd>Telescope yank_history<CR>', { silent = true })

map('n', 'g>', function()
  helper.open_buffer_to_execute('messages') -- This feature is not provided by telescope.nvim. use :messages instead
end, { silent = true })

--- Another AI Agents
map('n', '<leader>gc', helper.toggle_copilot_cli)
map('n', '<leader>Gc', helper.toggle_gemini_cli)

-- Options
map('n', '<C-h><C-w>', '<Cmd>setlocal wrap! wrap?<CR>', { silent = true })
map('n', '<C-h><C-c>', '<Cmd>setlocal cursorline! cursorline?<CR>', { silent = true })
map('n', '<C-h><C-r>', '<Cmd>setlocal relativenumber! relativenumber?<CR>', { silent = true })
map('n', '<C-h><C-l>', '<Cmd>setlocal list! list?<CR>', { silent = true })
map('n', '<C-h><C-n>', '<Cmd>setlocal number! number?<CR>', { silent = true })
map('n', '<C-h>v', helper.toggle_diagnostic_virtual_text, { silent = true })
map('n', '<C-h><C-d>', helper.toggle_diff, { silent = true })

map('n', '<C-h><C-v>', function()
  local verticaledit = vim.opt_local.virtualedit
  vim.opt_local.virtualedit = (verticaledit[1] == '' or #verticaledit == 0) and 'all' or ''
  vim.cmd('set virtualedit?')
end, { silent = true })

map('n', 'zm', function()
  local foldmethod = vim.wo.foldmethod
  vim.wo.foldmethod = (foldmethod == 'marker') and 'syntax' or 'marker'
  vim.cmd('set foldmethod?')
end, { silent = true })

-- Visualize a last pasted range
map('n', 'gp', function()
  local reg = string.sub(vim.fn.getregtype(), 1, 1)
  return s('`[{reg}`]', { reg = reg })
end, { expr = true })

-- Copy and paste
--- clipboard
map('n', '<leader>p', '"+p')
map('n', '<leader>P', '"+P')
map('n', '<leader>y', '"+y')
map('n', '<leader>Y', '"+yg_')
map('n', '<leader>dd', '"+dd')
map('n', '<leader>D', '"+D')
map('n', '<leader>d', '"+d')
map('n', '<leader>x', '"+x')

map('n', 'p', function()
  helper.run_with_virtual_keymaps('<Plug>(YankyPutAfter)')
  InitLua.hydra.yanky_ring:activate()
end)

map('n', 'P', function()
  helper.run_with_virtual_keymaps('<Plug>(YankyPutBefore)')
  InitLua.hydra.yanky_ring:activate()
end)

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

map('n', '"gp', function()
  put_filepath_relative_to_git_root('bellow')
end)

map('n', '"gP', function()
  put_filepath_relative_to_git_root('above')
end)

-- Operators and Objects
map('n', 'ga', helper.append_choose_surround_normal, { silent = true })
map('n', 'gs', helper.append_choose_surround_wide, { silent = true })
map('n', 'ds', helper.delete_mostly_inner_surround, { silent = true })
map('n', 'cs', helper.replace_mostly_inner_surround, { silent = true })
map('n', 'dijp', 'v<Plug>(textobj-jabraces-parens-i)x', { remap = true })
map('n', 'dajp', 'v<Plug>(textobj-jabraces-parens-a)x', { remap = true })
map('n', 'dijK', 'v<Plug>(textobj-jabraces-yama-kakko-i)x', { remap = true })
map('n', 'dajK', 'v<Plug>(textobj-jabraces-yama-kakko-a)x', { remap = true })
map('n', 'dij-k', 'v<Plug>(textobj-jabraces-double-kakko-i)x', { remap = true })
map('n', 'daj-k', 'v<Plug>(textobj-jabraces-double-kakko-a)x', { remap = true })
map('n', '.', '<Plug>(repeat-.)', { remap = true })
map('n', '<leader><leader>c', helper.camelize_or_uncamelize_current_word_as_repeatable, { silent = true })
--- Fake operator
map('n', '<C-v>ii', 'v<Plug>(textobj-indent-i)<C-v>ow', { remap = true }) -- Simular to vii, but select by <C-v>

--- Select inner line content (visual inner line)
map('n', 'vil', function()
  vim.cmd('normal! ^vg_')
end, { silent = true })

-- Hit characters
map('n', "'f", '<Plug>(fmap-forward-f)', { remap = true })
map('n', "'F", '<Plug>(fmap-backward-f)', { remap = true })
map('n', "'t", '<Plug>(fmap-forward-t)', { remap = true })
map('n', "'T", '<Plug>(fmap-backward-T)', { remap = true })
map('v', "'f", '<Plug>(fmap-forward-f)', { remap = true })
map('v', "'F", '<Plug>(fmap-backward-f)', { remap = true })
map('v', "'t", '<Plug>(fmap-forward-t)', { remap = true })
map('v', "'T", '<Plug>(fmap-backward-T)', { remap = true })

-- ALE and LSP
map('n', '<C-k><C-a>', '<Cmd>ALEToggle<CR>', { silent = true })
map('n', '[]', helper.open_diagnostic_detail)

map('n', '[c', function()
  helper.goto_diagnostic('previous')
end)

map('n', ']c', function()
  helper.goto_diagnostic('next')
end)

map('n', '<C-g><C-o>', vim.lsp.buf.hover, { silent = true }) -- Also can enter the floating window by stroke this keymap again when the floating window is opened
map('n', '<C-g>o', helper.show_lsp_diagnostic_float, { silent = true })
map('n', '<C-g><C-a>', vim.lsp.buf.code_action, { silent = true })
map('n', '<C-g><C-d>', vim.lsp.buf.definition, { silent = true })
map('n', '<C-g>d', vim.lsp.buf.declaration, { silent = true })
map('n', '<C-g><C-i>', vim.lsp.buf.implementation, { silent = true })
map('n', '<C-g><C-t>', vim.lsp.buf.type_definition, { silent = true })

-- TODO: これはなゆちゃんがおすすめしてくれたやつ。設定する？
-- map('n', '<C-k>', vim.lsp.buf.signature_help, { silent = true })
-- map('n', '<space>wa', vim.lsp.buf.add_workspace_folder, { silent = true })
-- map('n', '<space>wr', vim.lsp.buf.remove_workspace_folder, { silent = true })
-- map('n', '<space>wl', function()
--   print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
-- end, { silent = true })

-- Programming Utils
map('n', '<leader>R', '<Plug>(quickrepl-open)', { remap = true })
map('n', '<leader>r', '<Cmd>Jaq<CR>', { nowait = true }) -- `nowait`: 僕の<leader>rを持ち去ってるのはだれですか。`:verbose nmap <leader>r*`してもこのキーマップしか出ない

-- File Manupilation
map('n', '<leader>b', '<Cmd>MadoScratchBufferOpenFile md<CR>', { silent = true })
map('n', '<leader>B', '<Cmd>MadoScratchBufferOpenFileNext md<CR>', { silent = true })
map('n', '<leader><leader>b', ':<C-u>MadoScratchBufferOpenFile ')

map('n', '<leader><leader>B', function()
  vim.cmd('split ' .. InitLua.memo_path)
end, { silent = true })

-- File Editing
map('n', '<C-k><Space>', helper.remove_trailing_spaces, { silent = true })
map('n', '<Space><Space>', helper.compress_spaces, { silent = true })
map('n', '<leader><leader>s', 'vii:sort<CR>', { remap = true, silent = true })

map('n', '<C-k><C-s>', function()
  return s([[:%s/\m\C\<{word}\>//g<Left><Left>]], { word = vim.fn.expand('<cword>') })
end, { expr = true })

map('n', '<C-k>s', function()
  return s([[:%s/\m\C\<{word}\>/{word}/g<Left><left>]], { word = vim.fn.expand('<cword>') })
end, { expr = true })

-- Git Operations
map('n', '<leader>gs', '<Cmd>GinStatus<CR>', { silent = true })
map('n', '<leader>gl', '<Cmd>GitLog -100 --name-only<CR>', { silent = true }) -- Use my :GitLog due to :GinLog ignores arguments currently
map('n', '<leader>gL', '<Cmd>GitLog -100 --patch<CR>', { silent = true })
map('n', [[\gs]], '<Cmd>tabnew | GinStatus<CR>', { silent = true })

map('n', '<leader>go', function()
  git_log.open_buffer({ '-100', '--oneline', '--pretty=%h %ad %s', '--date=format:%Y-%m-%d %H:%M' })
end, { silent = true })

-- }}}
-- insert mode {{{

-- Fake digraphs
-- -> See below 'digraphs' section

-- Other than below
map('i', '<C-j>', '<CR>')
map('i', '<C-l>', '<Esc>')
map('i', '<C-a>', '<Right>')
map('i', '<C-k><C-k>', '<C-o>"_d$')
map('i', '<C-k><C-j>', '<Esc>:write<CR>', { silent = true })
map('i', '<C-k>J', '<Esc>:wall | echo "written all!"<CR>', { silent = true })
map('i', '<C-b>', network.fetch_webpage_title, { silent = true, expr = true })

-- Copy and paste
map('i', "<C-r>'", '<C-r>+')
--- Meaning "n"ame
map('i', '<C-r>n', '<C-r>=expand("%:t")<CR>')

-- Informations
map('i', '<C-g><Tab>', [[copilot#Accept("\<CR>")]], {
  expr = true,
  replace_keycodes = false,
})
map('i', '<C-g><C-n>', '<Plug>(copilot-next)', { remap = true })

-- File Editing
map('i', '<C-s>', function()
  ---@diagnostic disable-next-line: undefined-field --なんで見つからないんだろう？ もしかしたらtypesディレクトリに追加した方がいい？
  if not snip.expand_or_jumpable() then
    vim.notify('No snippet to expand or jump to', vim.log.levels.INFO)
    return
  end
  ---@diagnostic disable-next-line: undefined-field
  snip.expand_or_jump()
end, { silent = true })

-- }}}
-- select mode {{{

map('s', '<C-l>', '<Esc>')

-- File Editing
map('s', '<C-s>', function()
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

map('c', '<C-]>', [[\m\C\<\><Left><Left>]])
map('c', '<C-b>', '<Left>')
map('c', '<C-f>', '<Right>')
map('c', '<C-a>', '<Home>')
map('c', '<C-h>', '<BS>')
map('c', '<C-d>', '<Del>')
map('c', '<C-e>', '<End>')
map('c', '<C-k><C-k>', helper.remove_text_after_cursor, { expr = true })
map('c', '<C-l>', '<C-c>')
map('c', '<C-o>', '<Up>')
map('c', '<C-y>', '<Down>')
map('c', "<C-r>'", '<C-r>+')
-- Meaning "n"ame
map('c', '<C-r>n', '<C-r>=expand("%:t")<CR>')

-- TODO: 動いてない
-- kensaku-search.vim
-- map('c', '<CR>', '<Plug>(kensaku-search-replace)<CR>')

-- }}}
-- visual/operator mode {{{

-- Other than below
map('v', '<C-l>', '<Esc>')
map('v', 'g_', '$')
map('v', '<leader>p', '"+p')
map('v', '<leader>P', '"+P')
map('v', '<leader>y', '"+y')
map('v', '<leader>d', '"+d')
map('v', '<leader>x', '"+x')
map('v', '<leader>w', '<Plug>(openbrowser-open)', { remap = true })

-- Folds
map('v', 'zo', 'zogv')
map('v', 'zO', 'zOgv')

-- Don't select blanks
map('v', 'a"', '2i"')
map('o', 'a"', '2i"')
map('v', "a'", "2i'")
map('o', "a'", "2i'")
map('v', 'a`', '2i`')
map('o', 'a`', '2i`')

-- Quotes
map('v', 'ab', '2i`')
map('o', 'ab', '2i`')
map('v', 'ib', 'i`')
map('o', 'ib', 'i`')

-- Brackets
map('v', 'ap', 'a(')
map('o', 'ap', 'a(')
map('v', 'aP', 'a{')
map('o', 'aP', 'a{')
map('v', 'ak', 'a[')
map('o', 'ak', 'a[')
map('v', 'aK', 'a<')
map('o', 'aK', 'a<')
map('v', 'ip', 'i(')
map('o', 'ip', 'i(')
map('v', 'iP', 'i{')
map('o', 'iP', 'i{')
map('v', 'ik', 'i[')
map('o', 'ik', 'i[')
map('v', 'iK', 'i<')
map('o', 'iK', 'i<')

-- Operators and Objects
map('v', 'ga', '<Plug>(operator-surround-append)')
map('o', 'ga', '<Plug>(operator-surround-append)')
map('v', 'ai', '<Plug>(textobj-indent-a)')
map('v', 'ii', '<Plug>(textobj-indent-i)')
map('v', 'ijp', '<Plug>(textobj-jabraces-parens-i)')
map('v', 'ajp', '<Plug>(textobj-jabraces-parens-a)')
map('v', 'ijK', '<Plug>(textobj-jabraces-yama-kakko-i)')
map('v', 'ajK', '<Plug>(textobj-jabraces-yama-kakko-a)')
map('v', 'ij-k', '<Plug>(textobj-jabraces-double-kakko-i)')
map('v', 'aj-k', '<Plug>(textobj-jabraces-double-kakko-a)')
map('v', 'i_', '<Plug>(textobj-between-a)_')
map('v', 'a_', '<Plug>(textobj-between-i)_')
map('v', 'iB', '<Plug>(textobj-between-a)*') -- TODO: **を対象にしたい（vim-textobj-betweenは多分1-charのみ対応）
map('v', 'aB', '<Plug>(textobj-between-i)*')
-- TODO: 今度↑をvim-surround-operatorで置き換える（可能なら）
-- --- vim-operator-surround
-- map('v', 'ajp', '<Plug>(operator-surround-append)gajp')
-- map('o', 'ajp', '<Plug>(operator-surround-append)gajp')
--           { block = { '「', '」' }, motionwise = { 'char', 'line', 'block' }, keys = { 'jk' } },
-- map('v', '', '<Plug>(operator-surround-append)gajp')
-- map('o', 'ajp', '<Plug>(operator-surround-append)gajp')
--           { block = { '【', '】' }, motionwise = { 'char', 'line', 'block' }, keys = { 'jK' } },
-- map('v', 'ajp', '<Plug>(operator-surround-append)gajp')
-- map('o', 'ajp', '<Plug>(operator-surround-append)gajp')
--           { block = { '『', '』' }, motionwise = { 'char', 'line', 'block' }, keys = { 'j-k' } },
-- map('v', 'ajp', '<Plug>(operator-surround-append)gajp')
-- map('o', 'ajp', '<Plug>(operator-surround-append)gajp')

map('v', 'a_', function()
  return vim.fn['textobj#from_regexp#mapexpr']('[^A-Za-z0-9][A-Za-z0-9]\\+[^A-Za-z0-9]')
end, { expr = true })

map('v', 'i_', function()
  return vim.fn['textobj#from_regexp#mapexpr']('[A-Za-z0-9]\\+')
end, { expr = true })

map('v', 'al', function()
  return vim.fn['textobj#from_regexp#mapexpr']('^.*$')
end, { expr = true })

map('v', 'il', function()
  return vim.fn['textobj#from_regexp#mapexpr']('^\\s*\\zs.*\\ze.*$')
end, { expr = true })

map('o', 'il', function()
  return vim.fn['textobj#from_regexp#mapexpr']('^\\s*\\zs.*\\ze.*$')
end, { expr = true })

-- Programming Utils
map('v', '<leader>r', '<Plug>(quickrun)', { remap = true })

-- File Editing
map('v', '<leader><leader>s', ':sort<CR>')
map('v', '<leader><leader>c', '<Plug>(operator-camelize-toggle)', { remap = true })

-- }}}
-- terminal mode {{{

map('t', '<C-l>', [[<C-\><C-n>]])
map('t', [[<C-\><C-n>]], '<Esc>')
map('t', '<C-[>', '<Esc>')
map('t', '<C-]>', '<C-l>')

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
digraph ph 934    " Φ phi
digraph pi 960    " π pi
]])

-- fake digraphs
map('i', '<C-k>\\+', '＋')
map('i', '<C-k>\\-', '−')
map('i', '<C-k>\\=', '＝')
map('i', '<C-k>?=', '≒')
map('i', '<C-k>=~', '≅')
map('i', '<C-k>\\N', 'ℕ')
map('i', '<C-k>\\Z', 'ℤ')
map('i', '<C-k>\\R', 'ℝ')
map('i', '<C-k>\\Q', 'ℚ')
map('i', '<C-k>\\C', 'ℂ')
map('i', '<C-k>..', '◉')
map('i', '<C-k>\\|>', '↦')

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
