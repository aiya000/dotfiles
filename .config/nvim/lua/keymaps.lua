local helper = require('helper')
local fn = require('utils.functions')
local s = fn.s
local network = require('utils.network')

local function map(mode, lhs, rhs, opts)
  opts = opts or {}
  vim.keymap.set(mode, lhs, rhs, opts)
end

local function clear()
  vim.call('yankround#inactivate')
  vim.cmd('PreciousSwitch')
  helper.close_all_popups()
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

map('n', '<C-[>', clear, { silent = true })
map('n', '<Esc>', clear, { silent = true })
map('n', '<C-l>', clear, { silent = true })
map('n', '<C-k><C-l>', clear_deep)

map('n', '<C-k>o', '<Cmd>e! %<CR>', { silent = true })
map('n', '<leader><leader>B', function()
  vim.cmd(s('split {InitLua.memo_path}'))
end, { silent = true })
map('n', 'g*', '<Cmd>execute "silent! normal! *<C-o>"<CR>', { silent = true })
map('n', 'Q', function()
  helper.bufclose_filetype(InitLua.temporary_buftypes)
end, { silent = true })

-- folds
map('n', 'h', function()
  if vim.fn.foldclosed('.') > -1 then
    return 'zo'
  else
    return 'h'
  end
end, { expr = true })

map('n', 'l', function()
  if vim.fn.foldclosed('.') > -1 then
    return 'zo'
  else
    return 'l'
  end
end, { expr = true })

map('n', 'zj', 'zjzo')
map('n', 'zk', 'zkzo[zzt')

-- windows
-- <Space> prefix
map('n', '<Space>h', '<C-w>h')
map('n', '<Space>j', '<C-w>j')
map('n', '<Space>k', '<C-w>k')
map('n', '<Space>l', '<C-w>l')

-- gh prefix
map('n', 'ghR', '<C-w>r')
map('n', 'ghq', helper.hide_or_quit, { silent = true })
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

-- Disable defaults
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


map('n', '<leader>v', function()
  vim.cmd('vertical new')
  vim.fn.termopen(vim.env.SHELL)
end, { silent = true })
map('n', '<leader><leader>v', function()
  vim.cmd('new')  -- horizontal split
  vim.fn.termopen(vim.env.SHELL)
end, { silent = true })
map('n', '<leader>V', function()
  local cwd = helper.get_current_buffer_dir({ alt_dir = InitLua.git_root })
  vim.fn.termopen(vim.env.SHELL, { cwd = cwd })
end, { silent = true })
map('n', '<leader><leader>V', function()
  vim.cmd('tabnew')
  local cwd = helper.get_current_buffer_dir({ alt_dir = InitLua.git_root })
  vim.fn.termopen(vim.env.SHELL, { cwd = cwd })
end, { silent = true })

-- set
map('n', '<C-h><C-d>', function()
  helper.toggle_diff()
end, { silent = true })
map('n', '<C-h><C-v>', function()
  local ve = vim.opt_local.virtualedit:get()
  vim.opt_local.virtualedit = (ve[1] == '' or #ve == 0) and 'all' or ''
  vim.cmd('set virtualedit?')
end, { silent = true })
map('n', 'zm', function()
  local fm = vim.wo.foldmethod
  vim.wo.foldmethod = (fm == 'marker') and 'syntax' or 'marker'
  vim.cmd('set foldmethod?')
end, { silent = true })
map('n', '<C-h><C-w>', '<Cmd>setlocal wrap! wrap?<CR>', { silent = true })
map('n', '<C-h><C-c>', '<Cmd>setlocal cursorline! cursorline?<CR>', { silent = true })
map('n', '<C-h><C-r>', '<Cmd>setlocal relativenumber! relativenumber?<CR>', { silent = true })
map('n', '<C-h><C-l>', '<Cmd>setlocal list! list?<CR>', { silent = true })
map('n', '<C-h><C-n>', '<Cmd>setlocal number! number?<CR>', { silent = true })

-- Visualize a last pasted range
map('n', 'gp', function()
  local regtype = vim.fn.getregtype()
  return string.format('`[%s`]', string.sub(regtype, 1, 1))
end, { expr = true })

-- copy & paste
-- clipboard
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
  vim.cmd(s('put=system("git ls-files --full-name {vim.fn.expand("%")}")'))
end)
map('n', '"gP', function()
  vim.cmd(s('put!=system("git ls-files --full-name {vim.fn.expand("%")}")'))
end)

-- cr
map('n', '<CR>', 'o<Esc>')
map('n', '<C-j>', '<CR>', { remap = true })
map('n', '<C-m>', '<CR>', { remap = true })

-- lsp
map('n', '<C-g><C-o>', '<Cmd>LspHover<CR>', { silent = true })
map('n', '<C-g><C-a>', '<Cmd>LspCodeAction<CR>', { silent = true })
map('n', '<C-g><C-d>', '<Cmd>LspDefinition<CR>', { silent = true })
map('n', '<C-g><C-i>', '<Cmd>LspPeekImplementation<CR>', { silent = true })
map('n', '<C-g><C-t>', '<Cmd>LspPeekTypeDefinition<CR>', { silent = true })
map('n', '<C-g><C-g>', '<C-g>')

-- surround
map('n', 'ga', helper.append_choose_surround_normal, { silent = true })
map('n', 'gs', helper.append_choose_surround_wide, { silent = true })
map('n', 'ds', helper.delete_mostly_inner_surround, { silent = true })
map('n', 'cs', helper.replace_mostly_inner_surround, { silent = true })

-- others
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
map('n', '<C-k><Space>', helper.remove_trailing_spaces, { silent = true })
map('n', '<Space><Space>', helper.compress_spaces, { silent = true })
map('n', 'L', ':b ')
map('n', ':lp', fn.const(':lua print()<Left>'), { expr = true })
map('n', ':ev', fn.const(':e ' .. InitLua.path_at_started .. '/'), { expr = true })
map('n', ':eg', fn.const(':e ' .. (InitLua.git_root or '') .. '/'), { expr = true })
map('n', ':eb', function()
  return ':e ' .. vim.fn.expand('%:p:h') .. '/'
end, { expr = true })
map('n', ':h', fn.const(':h '), { expr = true })

map('n', '<C-k><C-s>', function()
  local word = vim.fn.expand('<cword>')
  return s(':%s/\\m\\C\\<{word}\\>//g<Left><Left>', { word = word })
end, { expr = true })

map('n', '<C-k>s', function()
  local word = vim.fn.expand('<cword>')
  return s(':%s/\\m\\C\\<{word}\\>/{word}/g<Left><left>', { word = word })
end, { expr = true })

-- }}}
-- insert mode {{{

-- fake digraphs
-- -> See below 'digraphs' section

-- others
map('i', '<C-j>', '<CR>')
map('i', '<C-a>', '<Right>')
map('i', '<C-k><C-k>', '<C-o>"_d$')
map('i', '<C-k><C-j>', '<Esc>:write<CR>', { silent = true })
map('i', '<C-k>J', '<Esc>:wall | echo "written all!"<CR>', { silent = true })

map('i', '<C-b>', network.fetch_webpage_title, { silent = true, expr = true })

map('i', "<C-r>'", '<C-r>+')
-- Execute iabbr and Escape
map('i', '<C-l>', '<Space><Backspace><Esc>')

-- Meaning "n"ame
map('i', '<C-r>n', '<C-r>=expand("%:t")<CR>')

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

-- }}}
-- visual/operator mode {{{

-- folds
map('v', 'zo', 'zogv')
map('v', 'zO', 'zOgv')

-- Don't select blanks
map('v', 'a"', '2i"')
map('o', 'a"', '2i"')
map('v', "a'", "2i'")
map('o', "a'", "2i'")
map('v', 'a`', '2i`')
map('o', 'a`', '2i`')

-- quotes
map('v', 'ab', '2i`')
map('o', 'ab', '2i`')
map('v', 'ib', 'i`')
map('o', 'ib', 'i`')

-- brackets
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

-- others
map('v', '<C-l>', '<Esc>')
map('v', '<leader><leader>s', ':sort<CR>')
map('v', 'g_', '$')
map('v', "'p", '"+p')
map('v', "'P", '"+P')
map('v', "'y", '"+y')
map('v', "'d", '"+d')
map('v', "'x", '"+x')

-- }}}
-- digraphs{{{

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

-- select mode
map('s', '<C-l>', '<Esc>')

-- terminal mode
map('t', '<C-l>', '<C-\\><C-n>')
map('t', '<C-\\><C-n>', '<Esc>')
map('t', '<C-[>', '<Esc>')
map('t', '<C-]>', '<C-l>')

-- typo abbreviations
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
-- Additional mappings from .vimrc migration {{{

-- File explorer mappings
map('n', '<leader>e', function() helper.toggle_explorer() end, { silent = true })
map('n', '<leader><leader>e', function() helper.open_explorer('split') end, { silent = true })
map('n', '<leader>E', function() helper.open_explorer('stay') end, { silent = true })
map('n', '<leader><leader>E', function() helper.open_explorer('tabnew') end, { silent = true })
map('n', '\\e', function() helper.toggle_explorer(InitLua.path_at_started) end, { silent = true })
map('n', '\\\\e', function() helper.open_explorer('split', InitLua.path_at_started) end, { silent = true })
map('n', '\\E', function() helper.open_explorer('stay', InitLua.path_at_started) end, { silent = true })
map('n', '\\\\E', function() helper.open_explorer('tabnew', InitLua.path_at_started) end, { silent = true })

-- ALE (Linter) mappings
map('n', '<C-k><C-a>', '<Cmd>ALEToggle<CR>', { silent = true })
map('n', '<C-k>a', function() helper.toggle_ale_at_buffer() end, { silent = true })
map('n', '[]', '<Cmd>ALEDetail<CR>')
map('n', '[c', '<Cmd>ALEPrevious<CR>')
map('n', ']c', '<Cmd>ALENext<CR>')

-- Indent guides toggle
map('n', '<C-h><C-i>', function() helper.toggle_indent_guides() end, { silent = true })

-- Weblio dictionary
map('n', '<leader>K', ':<C-u>Weblio <C-r>=expand("<cword>")<CR><CR>')
map('v', '<leader>K', '"zy:<C-u>Weblio <C-r>z<CR>')

-- Translation mappings
map('v', '<leader>k', '"zy:Translate <C-r>z<CR>', { silent = true })
map('v', '<leader><leader>k', ':DeeplTranslateToJaOpenBuffer<CR>', { silent = true })
map('v', '<leader><leader>K', ':DeeplTranslateToEnOpenBuffer<CR>', { silent = true })

-- OpenBrowser
map('n', '<leader>w', '<Plug>(openbrowser-open)', { remap = true })
map('v', '<leader>w', '<Plug>(openbrowser-open)', { remap = true })

-- QuickRun
map('n', '<leader>r', '<Plug>(quickrun)', { remap = true })
map('v', '<leader>r', '<Plug>(quickrun)', { remap = true })

-- Scratch buffer
map('n', '<leader>b', '<Cmd>ScratchBufferOpenFile md<CR>', { silent = true })
map('n', '<leader>B', '<Cmd>ScratchBufferOpenFileNext md<CR>', { silent = true })
map('n', '<leader><leader>b', ':<C-u>ScratchBufferOpenFile ')

-- Text objects
map('v', 'ai', '<Plug>(textobj-indent-a)', { remap = true })
map('v', 'ii', '<Plug>(textobj-indent-i)', { remap = true })
map('v', 'aB', '<Plug>(textobj-between-a)', { remap = true })
map('v', 'iB', '<Plug>(textobj-between-i)', { remap = true })
map('v', 'a*', '<Plug>(textobj-between-a)*', { remap = true })
map('v', 'i*', '<Plug>(textobj-between-i)*', { remap = true })

-- Japanese brackets text objects
map('v', 'ijp', '<Plug>(textobj-jabraces-parens-i)', { remap = true })
map('v', 'ajp', '<Plug>(textobj-jabraces-parens-a)', { remap = true })
map('v', 'ijK', '<Plug>(textobj-jabraces-yama-kakko-i)', { remap = true })
map('v', 'ajK', '<Plug>(textobj-jabraces-yama-kakko-a)', { remap = true })
map('v', 'ij-k', '<Plug>(textobj-jabraces-double-kakko-i)', { remap = true })
map('v', 'aj-k', '<Plug>(textobj-jabraces-double-kakko-a)', { remap = true })

-- Neosnippet
map('i', '<C-s>', function()
  return vim.fn['neosnippet#mappings#expand_or_jump_impl']()
end, { silent = true, expr = true })

-- incsearch
map('n', 'g/', '<Plug>(incsearch-stay)', { remap = true })

-- Visual star
map('v', 'g*', '<Plug>(visualstar-*)Nzz', { remap = true })

-- Anzu (search highlighting)
map('n', 'n', function()
  if vim.v.searchforward == 1 then
    return '<Plug>(anzu-n-with-echo)zv'
  else
    return '<Plug>(anzu-N-with-echo)zv'
  end
end, { expr = true })

map('n', 'N', function()
  if vim.v.searchforward == 1 then
    return '<Plug>(anzu-N-with-echo)zv'
  else
    return '<Plug>(anzu-n-with-echo)zv'
  end
end, { expr = true })

map('n', '*', '<Plug>(anzu-star-with-echo)zv', { remap = true })
map('n', '#', '<Plug>(anzu-sharp-with-echo)zv', { remap = true })

-- Copilot
map('i', '<C-g><Tab>', 'copilot#Accept("\\<CR>")', {
  expr = true,
  replace_keycodes = false,
})
map('i', '<C-]>', '<Plug>(copilot-next)', { remap = true })

-- Sort selected text
map('n', '<leader><leader>s', 'vii:sort<CR>', { silent = true })

-- DDU (fuzzy finder) mappings
map('n', '<C-k><C-e>', function() helper.ddu_start_from_input({ name = 'file_rec' }, '') end)
map('n', '<C-k>e', function() helper.ddu_start_from_input({ name = 'file_rec' }) end)
map('n', 'H', function()
  vim.fn['ddu#start']({ sources = { { name = 'help' } } })
end)
map('n', 'M', function() helper.ddu_start_from_input({ sources = { { name = 'mr' } } }) end)
-- map('n', ':h', function() helper.ddu_start_from_input({ sources = { { name = 'help' } } }) end)
map('n', '<C-k><C-f>', function()
  vim.fn['ddu#start']({ sources = { { name = 'rg', params = { input = vim.fn.input('Pattern: ') } } } })
end)

-- Tab navigation with wraparound
map('n', '<C-n>', function()
  return vim.fn['yankround#is_active']() == 1
    and '<Plug>(yankround-next)'
    or helper.tabnext_loop()
end, { silent = true })

map('n', '<C-p>', function()
  return vim.fn['yankround#is_active']() == 1
    and '<Plug>(yankround-prev)'
    or helper.tabprev_loop()
end, { silent = true })

map('n', 'P', '<Plug>(yankround-P)', { remap = true })
map('n', 'p', '<Plug>(yankround-p)', { remap = true })

-- UndoTree
map('n', '<leader>U', '<Cmd>UndotreeToggle<CR>', { silent = true })

-- F/T movement enhancement (fmap)
map('n', "'f", '<Plug>(fmap-forward-f)', { remap = true })
map('n', "'F", '<Plug>(fmap-backward-f)', { remap = true })
map('n', "'t", '<Plug>(fmap-forward-t)', { remap = true })
map('n', "'T", '<Plug>(fmap-backward-T)', { remap = true })
map('v', "'f", '<Plug>(fmap-forward-f)', { remap = true })
map('v', "'F", '<Plug>(fmap-backward-f)', { remap = true })
map('v', "'t", '<Plug>(fmap-forward-t)', { remap = true })
map('v', "'T", '<Plug>(fmap-backward-T)', { remap = true })

-- Camelize toggle
map('n', '<leader><leader>c', function()
  -- This requires a custom function implementation
  print('Camelize toggle - needs custom implementation')
end, { silent = true })
map('v', '<leader><leader>c', '<Plug>(operator-camelize-toggle)', { remap = true })

-- Repeat operation
map('n', '.', '<Plug>(repeat-.)', { remap = true })

-- QuickREPL
map('n', '<leader>R', '<Plug>(quickrepl-open)', { remap = true })

-- TODO: 動いてない
-- Kensaku (Japanese search)
map('c', '<CR>', '<Plug>(kensaku-search-replace)<CR>')

-- Text object extensions
map('v', 'a_', fn.const(vim.call('textobj#from_regexp#mapexpr', '[^A-Za-z0-9][A-Za-z0-9]\\+[^A-Za-z0-9]')), { expr = true })
map('v', 'i_', fn.const(vim.call('textobj#from_regexp#mapexpr', '[A-Za-z0-9]\\+')), { expr = true })
map('v', 'al', fn.const(vim.call('textobj#from_regexp#mapexpr', '^.*$')), { expr = true })
map('v', 'il', fn.const(vim.call('textobj#from_regexp#mapexpr', '^\\s*\\zs.*\\ze.*$')), { expr = true })

-- Select inner line content (visual inner line)
map('n', 'vil', function()
  vim.cmd('normal! ^vg_')
end, { silent = true })
map('o', 'il', fn.const(vim.call('textobj#from_regexp#mapexpr', '^\\s*\\zs.*\\ze.*$')), { expr = true })

-- Special buffer operations
map('n', 'g:', function() helper.open_buffer_to_execute('buffers') end, { silent = true })
map('n', 'g>', function() helper.open_buffer_to_execute('messages') end, { silent = true })
map('n', 'm:', function() helper.open_buffer_to_execute('marks') end, { silent = true })
map('n', 'q>', function() helper.open_buffer_to_execute('register') end, { silent = true })

-- Git operations (<leader>g* mappings)
map('n', '<leader>gs', '<Cmd>GStatus<CR>', { silent = true })
map('n', '<leader>gS', '<Cmd>GitShowViewer<CR>', { silent = true })
map('n', '<leader>gc', '<Cmd>GCommit<CR>', { silent = true })
map('n', '<leader>gC', '<Cmd>GCommitAmmend<CR>', { silent = true })
map('n', '<leader>ga', '<Cmd>GAddPatch<CR>', { silent = true })
map('n', '<leader>gl', '<Cmd>GLog<CR>', { silent = true })
map('n', '<leader>gL', '<Cmd>GLogPatch<CR>', { silent = true })
map('n', '<leader>go', '<Cmd>GLogOneline --pretty=\'%h %ad %s\' --date=\'format:%Y-%m-%d %H:%M\'<CR>', { silent = true })
map('n', '<leader>gd', '<Cmd>GDiff<CR>', { silent = true })
map('n', '<leader>gb', '<Cmd>GBranchAll<CR>', { silent = true }) -- Fixed typo: GBrahcnAll -> GBranchAll
map('n', '<leader>gt', '<Cmd>GLogTree<CR>', { silent = true })
map('n', '<leader>gT', '<Cmd>GLogTreeAll<CR>', { silent = true })

-- Git operations (\g* mappings - open in new tab)
map('n', '\\gs', '<Cmd>tabnew | GStatus<CR>', { silent = true })
map('n', '\\gl', '<Cmd>tabnew | GLog<CR>', { silent = true })
map('n', '\\gL', '<Cmd>tabnew | GLogPatch<CR>', { silent = true })
map('n', '\\go', '<Cmd>tabnew | GLogOneline --pretty=\'%h %ad %s\' --date=\'format:%Y-%m-%d %H:%M\'<CR>', { silent = true })

-- }}}

-- なぜかdirvishが開くので無効化
map('n', '-', '-')
