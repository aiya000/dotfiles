local nvim = require('nvim')

return {
  'aiya000/nvim-mado-scratch',
  -- dir = vim.fn.expand('~/Repository/nvim-mado-scratch'),
  -- dir = vim.fn.expand('~/Repository/nvim-mado-scratch/main'),
  dependencies = { 'nvim-lua/plenary.nvim' },
  config = function()
    require('mado-scratch').setup({
      default_file_ext = 'md',
      default_open_method = {
        method = 'float-aspect',
        scale = {
          width = 0.8,
          height = 0.8,
        },
      },
    })

    local augroup = vim.api.nvim_create_augroup('InitLuaPluginsMadoScratch', { clear = true })

    vim.api.nvim_create_autocmd('User', {
      group = augroup,
      pattern = 'MadoScratchBufferOpened',
      callback = function()
        nvim.keymaps_set('n', nvim.escaping_keys, '<Nop>', { buffer = true })
        nvim.keymaps_set('n', { '<C-l><C-l>', '<Esc><Esc>', '<C-[><C-[>' }, '<Cmd>q<CR>', { buffer = true })
      end,
    })

    vim.api.nvim_create_autocmd('User', {
      group = augroup,
      pattern = 'MadoScratchBufferPreClosed',
      callback = function()
        vim.cmd.write()
      end,
    })
  end,
}
