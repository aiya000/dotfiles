local helper = require('helper')
local fn = require('utils.functions')
local s = fn.s
local network = require('utils.network')
local git_log = require('git-log')
local telescope = require('telescope.builtin')

local function map(mode, lhs, rhs, opts)
  opts = opts or {}
  vim.keymap.set(mode, lhs, rhs, opts)
end

local function clear()
  vim.cmd('PreciousSwitch')
  helper.close_all_popups()
  require('notify').dismiss({ silent = true, pending = true })
  vim.cmd('nohlsearch')
end

local function clear_deep()
  print('clearing...')
  vim.cmd('PreciousReset') -- NOTE: This is a little heavy
  clear()
  print('cleared!')
end

local function clear_and_write()
  clear()
  vim.cmd('write')
end

-- normal mode {{{

-- Allow keymaps like <C-c>{foo}, and {bar}<C-c>
map('n', '<C-c>', '<NOP>')
map('n', '<C-c><C-c>', '<C-c>')

-- Allow keymaps like <C-g>{foo}, and {bar}<C-g>
map('n', '<C-g>', '<NOP>')
map('n', '<C-g><C-g>', '<C-g>')

-- Other than below
map('n', '<CR>', 'o<Esc>')
map('n', '<C-j>', '<CR>', { remap = true })
map('n', '<C-m>', '<CR>', { remap = true })
map('n', '<C-[>', clear, { silent = true })
map('n', '<Esc>', clear, { silent = true })
map('n', '<C-l>', clear, { silent = true })
map('n', '<C-k><C-l>', clear_deep)
map('n', '<C-k>o', '<Cmd>e! %<CR>', { silent = true })
map('n', '-', '-') -- デフォルト（？）で、なぜかdirvishが開くので、無効化
map('n', 'gG', 'ggVG')
map('n', '(', '(zv')
map('n', ')', ')zv')
map('n', '::', ':%s/')
map('n', '<C-k><C-j>', clear_and_write)
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

map('n', ':lp', ':lua print()<Left>')
-- NOTE: ↓以下だとNeovim起動時にエラーが出るの、なんでだろう {{{
--
-- map('n', ':ev', s(':e {path_at_started}/', { path_at_started = InitLua.path_at_started }))
-- map('n', ':ev', function()
--   return s(':e {path_at_started}/', { path_at_started = InitLua.path_at_started })
-- end, { expr = true })
--
-- Error detected while processing .config/nvim/init.lua:
-- E5113: Error while calling lua chunk: vim/keymap.lua:0: opts: expected table, got number
-- stack traceback:
--         [C]: in function 'error'
--         vim/shared.lua: in function 'validate'
--         vim/keymap.lua: in function 'set'
--         .config/nvim/lua/keymaps.lua:8: in function 'map'
--         .config/nvim/lua/keymaps.lua:232: in main chunk
--         [C]: in function 'require'
--         .dotfiles/.config/nvim/init.lua:238: in main chunk
--
-- }}}
map('n', ':ev', ':e ' .. InitLua.path_at_started .. '/')

map('n', ':eg', function()
  return s(':e {git_root}/', { git_root = InitLua.git_root })
end, { expr = true })

map('n', ':eb', function()
  return s(':e {current_buffer_directory}/', { current_buffer_directory = vim.fn.expand('%:p:h') })
end, { expr = true })

-- Search
local function try_show_search_number_or_do_nothing()
  local ok, hlslens = pcall(require, 'hlslens')
  if not ok then
    return
  end
  hlslens.start()
end

--- Search cword without moving
map('n', 'g*', function ()
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
map('n', 'Q', function()
  helper.bufclose_filetype(InitLua.temporary_buftypes)
end, { silent = true })

-- Foldings
map('n', 'h', function()
  return vim.fn.foldclosed('.') > -1
    and 'zo'
    or 'h'
end, { expr = true })

map('n', 'l', function()
  return vim.fn.foldclosed('.') > -1
    and 'zo'
    or 'l'
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

--- tabs navigation (yanky handles yank history with different keys)
-- TODO: <C-n>/<C-p> がyanky.nvimのYankyNextEntry/YankyPreviousEntryと競合しています
-- タブ移動を別のキーに変更するか、yanky.nvimの設定を変更してください
map('n', '<C-n>', helper.tabnext_loop_or_yanky_next, { silent = true })
map('n', '<C-p>', helper.tabprev_loop_or_yanky_prev, { silent = true })

-- :terminal
map('n', '<leader>v', function()
  vim.cmd('vertical new')
  helper.termopen_shell()
end, { silent = true })

map('n', '<leader><leader>v', function()
  vim.cmd('new')
  helper.termopen_shell()
end, { silent = true })

map('n', '<leader>V', function()
  vim.fn.termopen(vim.env.SHELL, {
    cwd = helper.get_current_buffer_dir({ alt_dir = InitLua.git_root })
  })
end, { silent = true })

map('n', '<leader><leader>V', function()
  vim.cmd('tabnew')
  vim.fn.termopen(vim.env.SHELL, {
    cwd = helper.get_current_buffer_dir({ alt_dir = InitLua.git_root })
  })
end, { silent = true })

-- File explorer
map('n', '<leader>e', function() helper.toggle_explorer() end, { silent = true })
map('n', '<leader><leader>e', function() helper.open_explorer('split') end, { silent = true })
map('n', '<leader>E', function() helper.open_explorer('stay') end, { silent = true })
map('n', '<leader><leader>E', function() helper.open_explorer('tabnew') end, { silent = true })
map('n', '\\e', function() helper.toggle_explorer(InitLua.path_at_started) end, { silent = true })
map('n', '\\\\e', function() helper.open_explorer('split', InitLua.path_at_started) end, { silent = true })
map('n', '\\E', function() helper.open_explorer('stay', InitLua.path_at_started) end, { silent = true })
map('n', '\\\\E', function() helper.open_explorer('tabnew', InitLua.path_at_started) end, { silent = true })

-- List up
map('n', '<C-k><C-e>', telescope.find_files) -- TODO: もしパフォーマンスが遅ければ、このキーマッピングはカレントディレクトリ以下のみを表示して、プロジェクトルート以下の表示（`InitLua.git_root or InitLua.path_at_started`）は以下の<C-k>eに分担させる
-- map('n', '<C-k>e', telescope.find_files)
map('n', '<C-k><C-f>', telescope.lsp_document_symbols)
map('n', ':h<Space>', telescope.help_tags)
map('n', 'L', telescope.buffers)
map('n', 'H', telescope.live_grep)
map('n', 'M', telescope.oldfiles)

-- Options
map('n', '<C-h><C-w>', '<Cmd>setlocal wrap! wrap?<CR>', { silent = true })
map('n', '<C-h><C-c>', '<Cmd>setlocal cursorline! cursorline?<CR>', { silent = true })
map('n', '<C-h><C-r>', '<Cmd>setlocal relativenumber! relativenumber?<CR>', { silent = true })
map('n', '<C-h><C-l>', '<Cmd>setlocal list! list?<CR>', { silent = true })
map('n', '<C-h><C-n>', '<Cmd>setlocal number! number?<CR>', { silent = true })

map('n', '<C-h><C-d>', function()
  helper.toggle_diff()
end, { silent = true })

map('n', '<C-h><C-v>', function()
  local verticaledit = vim.opt_local.virtualedit:get()
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
  return s('`[%s`]', { reg = string.sub(vim.fn.getregtype(), 1, 1) })
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

-- TODO: なんでgit使ってるんだっけ？ ちょっと考えてみて、gitを使う意図がなければ、直す（なゆちゃんが誤解していたかもしれないので、なぜかgitが挟まっていてもおかしくない）
-- Put the relative path of a current file
map('n', '"gp', function()
  vim.cmd(s('put=system("git ls-files --full-name {filename}")', { filename = vim.fn.expand('%') }))
end)

map('n', '"gP', function()
  vim.cmd(s('put!=system("git ls-files --full-name {filename}")', { filename = vim.fn.expand('%') }))
end)

-- Operators and Objects
map('n', 'ga', helper.append_choose_surround_normal, { silent = true })
map('n', 'gs', helper.append_choose_surround_wide, { silent = true })
map('n', 'ds', helper.delete_mostly_inner_surround, { silent = true })
map('n', 'cs', helper.replace_mostly_inner_surround, { silent = true })
map('n', '.', '<Plug>(repeat-.)', { remap = true })
map('n', '<leader><leader>c', helper.camelize_or_uncamelize_current_word_as_repeatable, { silent = true })

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

-- Informations
map('n', '<C-k><C-a>', '<Cmd>ALEToggle<CR>', { silent = true })
map('n', '<C-k>a', function() helper.toggle_ale_at_buffer() end, { silent = true })
map('n', '[]', '<Cmd>ALEDetail<CR>')
map('n', '[c', '<Cmd>ALEPrevious<CR>')
map('n', ']c', '<Cmd>ALENext<CR>')
map('n', '<C-g><C-o>', '<Cmd>LspHover<CR>', { silent = true })
map('n', '<C-g><C-a>', '<Cmd>LspCodeAction<CR>', { silent = true })
map('n', '<C-g><C-d>', '<Cmd>LspDefinition<CR>', { silent = true })
map('n', '<C-g><C-i>', '<Cmd>LspPeekImplementation<CR>', { silent = true })
map('n', '<C-g><C-t>', '<Cmd>LspPeekTypeDefinition<CR>', { silent = true })

-- Translation
map('n', '<leader>K', ':<C-u>Weblio <C-r>=expand("<cword>")<CR><CR>')

-- Programming Utils
map('n', '<leader>R', '<Plug>(quickrepl-open)', { remap = true })
map('n', '<leader>r', '<Plug>(quickrun)', { remap = true })

-- File Manupilation
map('n', '<leader>b', '<Cmd>ScratchBufferOpenFile md<CR>', { silent = true })
map('n', '<leader>B', '<Cmd>ScratchBufferOpenFileNext md<CR>', { silent = true })
map('n', '<leader><leader>b', ':<C-u>ScratchBufferOpenFile ')

-- File Editing
map('n', '<C-k><Space>', helper.remove_trailing_spaces, { silent = true })
map('n', '<Space><Space>', helper.compress_spaces, { silent = true })
map('n', '<leader><leader>s', 'vii:sort<CR>', { silent = true })

map('n', '<C-k><C-s>', function()
  return s(':%s/\\m\\C\\<{word}\\>//g<Left><Left>', { word = vim.fn.expand('<cword>') })
end, { expr = true })

map('n', '<C-k>s', function()
  return s(':%s/\\m\\C\\<{word}\\>/{word}/g<Left><left>', { word = vim.fn.expand('<cword>') })
end, { expr = true })

-- Git Operations
map('n', '<leader>gs', '<Cmd>GinStatus<CR>', { silent = true })
map('n', '<leader>gl', '<Cmd>GitLog -100 --name-only<CR>', { silent = true }) -- Use my :GitLog due to :GinLog ignores arguments currently
map('n', '<leader>gL', '<Cmd>GitLog -100 --patch<CR>', { silent = true })
map('n', '\\gs', '<Cmd>tabnew | GinStatus<CR>', { silent = true })

map('n', '<leader>go', function()
  git_log.open_buffer({'-100', '--oneline', '--pretty=%h %ad %s', '--date=format:%Y-%m-%d %H:%M'})
end, { silent = true })

-- Open list up commands with new temporary buffer
map('n', 'g:', function()
  helper.open_buffer_to_execute('buffers')
end, { silent = true })

map('n', 'g>', function()
  helper.open_buffer_to_execute('messages')
end, { silent = true })

map('n', 'm:', function()
  helper.open_buffer_to_execute('marks')
end, { silent = true })

map('n', 'q>', function()
  helper.open_buffer_to_execute('register')
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
map('i', '<C-g><Tab>', 'copilot#Accept("\\<CR>")', {
  expr = true,
  replace_keycodes = false,
})
map('i', '<C-g><C-n>', '<Plug>(copilot-next)', { remap = true })

-- File Editing
map('i', '<C-s>', function()
  return vim.fn['neosnippet#mappings#expand_or_jump_impl']()
end, { silent = true, expr = true })

-- }}}
-- command-line mode {{{

map('c', '<C-]>', '\\m\\C\\<\\><Left><Left>')
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
map('v', "'p", '"+p')
map('v', "'P", '"+P')
map('v', "'y", '"+y')
map('v', "'d", '"+d')
map('v', "'x", '"+x')
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
map('v', 'ga', '<Plug>(operator-surround-append)', { silent = true })
map('o', 'ga', '<Plug>(operator-surround-append)', { silent = true })
map('v', 'ai', '<Plug>(textobj-indent-a)', { remap = true })
map('v', 'ii', '<Plug>(textobj-indent-i)', { remap = true })
map('v', 'aB', '<Plug>(textobj-between-a)', { remap = true })
map('v', 'iB', '<Plug>(textobj-between-i)', { remap = true })
map('v', 'a*', '<Plug>(textobj-between-a)*', { remap = true })
map('v', 'i*', '<Plug>(textobj-between-i)*', { remap = true })

--- TODO: operator-surroundで置き換え
map('v', 'ijp', '<Plug>(textobj-jabraces-parens-i)', { remap = true })
map('v', 'ajp', '<Plug>(textobj-jabraces-parens-a)', { remap = true })
map('v', 'ijK', '<Plug>(textobj-jabraces-yama-kakko-i)', { remap = true })
map('v', 'ajK', '<Plug>(textobj-jabraces-yama-kakko-a)', { remap = true })
map('v', 'ij-k', '<Plug>(textobj-jabraces-double-kakko-i)', { remap = true })
map('v', 'aj-k', '<Plug>(textobj-jabraces-double-kakko-a)', { remap = true })

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

-- Translation
map('v', '<leader>k', '"zy:Translate <C-r>z<CR>', { silent = true })
map('v', '<leader>K', '"zy:<C-u>Weblio <C-r>z<CR>')
map('v', '<leader><leader>k', ':DeeplTranslateToJaOpenBuffer<CR>', { silent = true })
map('v', '<leader><leader>K', ':DeeplTranslateToEnOpenBuffer<CR>', { silent = true })

-- Programming Utils
map('v', '<leader>r', '<Plug>(quickrun)', { remap = true })

-- File Editing
map('v', '<leader><leader>s', ':sort<CR>')
map('v', '<leader><leader>c', '<Plug>(operator-camelize-toggle)', { remap = true })

-- }}}
-- terminal mode {{{

map('t', '<C-l>', '<C-\\><C-n>')
map('t', '<C-\\><C-n>', '<Esc>')
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
-- select mode {{{

map('s', '<C-l>', '<Esc>')

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
