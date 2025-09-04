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

---surround operations (migrated from ~/.vim/autoload/vimrc.vim)
local function get_current_obj_keys()
  local surrounds = vim.g['operator#surround#blocks']['-'] or {}
  local filetype_surrounds = vim.g['operator#surround#blocks'][vim.bo.filetype] or {}

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
    if char == '\027' or char == '\003' or char == '' then
      return nil
    end
    stroke = stroke .. char
  end
  return stroke
end

local function append_choose_surround(visualizer)
  vim.call('dein#source', 'vim-operator-surround')
  local obj_keys = get_current_obj_keys()
  local obj_key = input_obj_key_of(obj_keys)
  if obj_key == nil then
    print('Cancelled')
    return nil
  end

  -- Execute the normal command
  vim.cmd('normal ' .. visualizer .. '\\<Plug>(operator-surround-append)' .. obj_key)
  return obj_key
end

local function delete_mostly_inner_surround()
  vim.call('dein#source', 'vim-operator-surround')
  local obj_keys = get_current_obj_keys()
  local obj_key = input_obj_key_of(obj_keys)
  if obj_key == nil then
    print('Cancelled')
    return
  end

  -- TODO: Workaround. For some reason 'B' ('**foo**') cannot be deleted
  if obj_key == 'B' then
    vim.cmd('normal! d\\<Plug>(textobj-between-i)*')
    vim.cmd('s/\\*\\*\\*\\*/' .. vim.fn.getreg('"') .. '/')
    print('**deleted**')
    return
  end

  vim.cmd('normal! va' .. obj_key .. '\\<Plug>(operator-surround-delete)')
  vim.fn['repeat#set']('\\<Plug>(vimrc-surround-delete-mostly-inner)' .. obj_key)
end

local function replace_mostly_inner_surround()
  vim.call('dein#source', 'vim-operator-surround')
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

  vim.cmd('normal! va' .. obj_key_from .. '\\<Plug>(operator-surround-replace)' .. obj_key_to)
  vim.fn['repeat#set']('\\<Plug>(vimrc-surround-replace-mostly-inner)' .. obj_key_from .. obj_key_to)
end

local function append_choose_surround_normal()
  local obj_key = append_choose_surround('viw')
  if obj_key then
    vim.fn['repeat#set']('\\<Plug>(vimrc-surround-append-choice)' .. obj_key)
  end
end

local function append_choose_surround_wide()
  local obj_key = append_choose_surround('viW')
  if obj_key then
    vim.fn['repeat#set']('\\<Plug>(vimrc-surround-append-choice-wide)' .. obj_key)
  end
end

-- normal mode {{{

-- Allow keymaps like <C-c>{foo}, and {bar}<C-c>
map('n', '<C-c>', '<NOP>')
map('n', '<C-c><C-c>', '<C-c>')

map('n', '<C-[>', clear, { silent = true })
map('n', '<Esc>', clear, { silent = true })
map('n', '<C-l>', clear, { silent = true })
map('n', '<C-k><C-l>', clear_deep)

-- listup
map('n', 'g:', '<Cmd>buffers<CR>', { silent = true })
map('n', 'g>', '<Cmd>messages<CR>', { silent = true })
map('n', 'm:', '<Cmd>marks<CR>', { silent = true })
map('n', 'q>', '<Cmd>registers<CR>', { silent = true })

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

-- TODO: luaで書いている時点でVimとNeovimの際を吸収する必要ないから、この関数は必要ないかも？
-- :terminal
local function open_terminal(options)
  local default_opts = {
    vertical = true,
    cwd = helper.get_current_buffer_dir({ alt_dir = InitLua.git_root }),
  }
  local opts = vim.tbl_extend('force', default_opts, options or {})

  if opts.vertical then
    vim.cmd('vsplit')
  end

  if opts.curwin then
    vim.cmd('terminal')
  else
    vim.cmd('terminal')
  end

  if opts.cwd then
    vim.cmd(s('lcd {opts.cwd}'))
  end
end

map('n', '<leader>v', function()
  open_terminal({ vertical = true })
end, { silent = true })
map('n', '<leader><leader>v', function()
  open_terminal({ vertical = false })
end, { silent = true })
map('n', '<leader>V', function()
  open_terminal({ curwin = true })
end, { silent = true })
map('n', '<leader><leader>V', function()
  vim.cmd('tabnew')
  open_terminal({ curwin = true })
end, { silent = true })

-- set
map('n', '<C-h><C-d>', function()
  require('vimrc').toggle_diff()
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
  return s('`[{string.sub(regtype, 1, 1)}`]')
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

-- Put the relative path of a current file
map('n', '"gp', function()
  vim.cmd(s('put=system("git ls-files --full-name {vim.fn.expand("%")}")'))
end)
map('n', '"gP', function()
  vim.cmd(s('put!=system("git ls-files --full-name {vim.fn.expand("%")}")'))
end)

-- cr
map('n', '<C-j>', '<CR>')
map('n', '<C-m>', '<CR>')
map('n', '<CR>', 'o<Esc>')

-- lsp
map('n', '<C-g><C-o>', '<Cmd>LspHover<CR>', { silent = true })
map('n', '<C-g><C-a>', '<Cmd>LspCodeAction<CR>', { silent = true })
map('n', '<C-g><C-d>', '<Cmd>LspDefinition<CR>', { silent = true })
map('n', '<C-g><C-i>', '<Cmd>LspPeekImplementation<CR>', { silent = true })
map('n', '<C-g><C-t>', '<Cmd>LspPeekTypeDefinition<CR>', { silent = true })
map('n', '<C-g><C-g>', '<C-g>')

-- surround
map('n', 'ga', append_choose_surround_normal, { silent = true })
map('n', 'gs', append_choose_surround_wide, { silent = true })
map('n', 'ds', delete_mostly_inner_surround, { silent = true })
map('n', 'cs', replace_mostly_inner_surround, { silent = true })

-- others
map('n', 'gG', 'ggVG')
map('n', '(', '(zv')
map('n', ')', ')zv')
map('n', '::', ':%s/')
map('n', ':ev', function()
  return ':e ' .. InitLua.path_at_started .. '/'
end, { expr = true })
map('n', ':eg', function()
  return ':e ' .. (InitLua.git_root or '') .. '/'
end, { expr = true })
map('n', ':eb', function()
  return ':e ' .. vim.fn.expand('%:p:h') .. '/'
end, { expr = true })
map('n', '<C-]>', 'g<C-]>')
map('n', '<C-k><C-s>', function()
  local word = vim.fn.expand('<cword>')
  return string.format(':%%s/\\m\\C\\<%s\\>//g\027\027', word)
end, { expr = true })
map('n', '<C-k>s', function()
  local word = vim.fn.expand('<cword>')
  return string.format(':%%s/\\m\\C\\<%s\\>/%s/g\027\027', word, word)
end, { expr = true })
map('n', '<C-k><C-j>', clear_and_write)
map('n', '<C-k><Space>', function()
  require('vimrc').remove_trailing_spaces()
end, { silent = true })
map('n', '<C-k>J', '<Cmd>wall | echo "written all !"<CR>', { silent = true })
map('n', '<Space><Space>', function()
  require('vimrc').compress_spaces()
end, { silent = true })
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
