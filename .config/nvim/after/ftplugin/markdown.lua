local helper = require('helper')

vim.opt_local.tabstop = 4
vim.opt_local.shiftwidth = 4
vim.opt_local.conceallevel = 0
vim.opt_local.commentstring = ' <!-- %s -->'
vim.opt_local.completefunc = 'github_complete#complete'

---Finds free port for grip server
---@param port integer
local function find_free_port(port)
  local result = vim.system("ss -tuln | grep ':" .. port .. "' | wc -l"):wait()
  if result.code ~= 0 then
    error('Failed to check port: ' .. result.stderr)
  end
  if vim.fn.trim(result.stdout) == '0' then
    return port
  end
  return find_free_port(port + 1)
end

local function start_grip()
  local token = vim.env.DOTFILES_PRIVATE_GITHUB_GRIP_TOKEN or ''
  local token_option = (token == '') and '' or ('--pass ' .. token)
  local port = find_free_port(25252)

  local ok, err = pcall(function()
    vim.fn.termopen(
      'grip ' .. token_option .. ' ' .. vim.fn.fnameescape(vim.fn.expand('%:p')) .. ' ' .. port,
      {
        vertical = true, -- TODO: ここらへんのオプションをVimのterm_start()から移行できてないので、無視されると思う。移行する
        hidden = true,
        term_finish = 'close',
      }
    )
    -- NOTE: なぜか`setl nonumber norelativenumber nolist`になるので、とりあえず直打ちで直している
    -- TODO: なんでこうなるのか調査して、修正する
    vim.opt_local.number = true
    vim.opt_local.relativenumber = true
    vim.opt_local.list = true
  end)

  if not ok then
    vim.notify('grip error: ' .. tostring(err), vim.log.levels.ERROR)
  end

  vim.fn.system(InitLua.open_on_gui .. ' http://localhost:' .. port)
end

vim.keymap.set('n', '<localleader>r', function()
  vim.cmd('PrevimOpen')
end, { buffer = true, silent = true })

vim.keymap.set('n', '<localleader><localleader>d', function()
  vim.cmd('write')
  vim.cmd('!doctoc ' .. vim.fn.expand('%'))
  vim.cmd('edit ' .. vim.fn.expand('%'))
end, { buffer = true, silent = true })

vim.keymap.set('n', '<localleader><localleader>r', start_grip, { buffer = true, silent = true })

-- TODO: Do 'gg' after glow finished
vim.keymap.set('n', '<localleader><localleader>R', function()
  vim.fn.termopen(
    'glow ' .. vim.fn.fnameescape(vim.fn.expand('%:p')),
    { vertical = true }
  )
end, { buffer = true, silent = true })

-- TODO: ちゃんと.vimrcと同様に、lsp_documentSymbolあたりを使う。可能ならここで<C-k><C-f>を押すとオーバーライドするよりも、lspを導入することで済むなら、そちらの方がよい
local function open_ddu_section_list()
  -- TODO: なんかここ効いてないので直す
  vim.cmd('normal! gg')
  local ok, err = pcall(function()
    helper.ddu_start_from_input({
      sources = {{ name = 'line' }},
      sourceOptions = {
        _ = {
          matchers = {'matcher_regex'},
        },
      },
    }, '^#+ ')
  end)
  -- TODO: なんかここ効いてないので直す
  vim.cmd('normal! \\<C-o>')
  if not ok then
    vim.notify(err, vim.log.levels.ERROR)
  end
end

vim.keymap.set('n', '<C-k><C-f>', open_ddu_section_list, { buffer = true, silent = true })

vim.cmd('syntax sync fromstart')
