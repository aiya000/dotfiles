local nvim = require('nvim')

local augroup = vim.api.nvim_create_augroup('InitLuaPluginsMadoScratch', { clear = true })

---@param bufnr integer
local function set_float_window_keymaps(bufnr)
  -- Avoid accidental exits
  nvim.keymaps_set('n', nvim.escaping_keys, '<Nop>', { buffer = bufnr })
  nvim.keymaps_set('n', { '<C-l><C-l>', '<Esc><Esc>', '<C-[><C-[>' }, '<Cmd>q<CR>', { buffer = bufnr })
end

---@param bufnr integer
local function unset_float_window_keymaps(bufnr)
  for _, key in ipairs(list.concat(nvim.escaping_keys, { '<C-l><C-l>', '<Esc><Esc>', '<C-[><C-[>' })) do
    pcall(vim.keymap.del, 'n', key, { buffer = bufnr }) -- NOTE: Why error occurs here? (if pcall is not used)
  end
end

---Enables opts because float windows defaults to nonumber and norelativenumber
local function set_buffer_opts()
  vim.opt.number = true
  vim.opt.relativenumber = true
end

---もしfloat windowで開いた場合、float window向けのキーマッピングを設定する。
---もしそうでなくfloat window内で:vspや:spをして、non floating windowで開き直した場合、
---float window向けのキーマッピングなしでバッファウィンドウを開く。
---@param bufnr integer
local function manage_buffer_keymaps(bufnr)
  local float_winid = vim.api.nvim_get_current_win()
  vim.api.nvim_create_autocmd('WinNew', {
    buffer = bufnr,
    group = augroup,
    callback = function()
      vim.schedule(function()
        if nvim.is_in_float_window() then
          set_float_window_keymaps(bufnr)
        elseif vim.api.nvim_win_is_valid(float_winid) then -- if not float window but previous float window exists
          vim.api.nvim_win_close(float_winid, false)
          unset_float_window_keymaps(bufnr)
        else
          unset_float_window_keymaps(bufnr)
        end
      end)
    end,
  })
end

local function setup_float_window()
  if not nvim.is_in_float_window() then
    return
  end

  local bufnr = vim.api.nvim_get_current_buf()
  set_float_window_keymaps(bufnr)
  set_buffer_opts()
  manage_buffer_keymaps(bufnr)
end

return {
  -- 'aiya000/nvim-mado-scratch',
  -- dir = vim.fn.expand('~/Repository/nvim-mado-scratch'),
  dir = vim.fn.expand('~/Repository/nvim-mado-scratch/main'),
  -- dir = vim.fn.expand('~/Repository/nvim-mado-scratch/feature/default_open_behavior'),
  dependencies = {
    'nvim-lua/plenary.nvim',
    'MunifTanjim/nui.nvim',
  },
  config = function()
    require('mado-scratch').setup({
      default_file_ext = 'md',
      default_open_method = 'vsp',
      default_open_params = {
        ['float-aspect'] = {
          scale = {
            width = 0.8,
            height = 0.8,
          },
        },
        vsp = {
          width = 'no-auto-resize',
        },
      },
    file_pattern = {
      when_file_buffer = '~/tmp/scratch-%d',
    },
    })

    vim.api.nvim_create_autocmd('User', {
      pattern = 'MadoScratchBufferOpened',
      group = augroup,
      callback = setup_float_window,
    })

    vim.api.nvim_create_autocmd('User', {
      group = augroup,
      pattern = 'MadoScratchBufferPreClosed',
      callback = function()
        vim.cmd.write({ bang = true })
      end,
    })
  end,
}
