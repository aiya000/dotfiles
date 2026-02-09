local nvim = require('nvim')

return {
  -- 'aiya000/nvim-mado-scratch',
  -- dir = vim.fn.expand('~/Repository/nvim-mado-scratch'),
  dir = vim.fn.expand('~/Repository/nvim-mado-scratch/main'),
  -- dir = vim.fn.expand('~/Repository/nvim-mado-scratch/copilot/fix-nvim-madoscratch-error'),
  dependencies = { 'nvim-lua/plenary.nvim' },
  config = function()
    require('mado-scratch').setup({
      default_file_ext = 'md',
      default_open_method = 'float-aspect',
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
    })

    local augroup = vim.api.nvim_create_augroup('InitLuaPluginsMadoScratch', { clear = true })

    vim.api.nvim_create_autocmd('User', {
      group = augroup,
      pattern = 'MadoScratchBufferOpened',
      callback = function()
        if nvim.is_in_float_window() then
          -- Float windows defaults to nonumber and norelativenumber, so enable them here
          vim.opt.number = true
          vim.opt.relativenumber = true

          -- Avoid accidental exits
          nvim.keymaps_set('n', nvim.escaping_keys, '<Nop>', { buffer = true })
          nvim.keymaps_set('n', { '<C-l><C-l>', '<Esc><Esc>', '<C-[><C-[>' }, '<Cmd>q<CR>', { buffer = true })
        end
      end,
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
