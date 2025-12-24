return {
  'aiya000/nvim-mado-scratch',
  -- dir = vim.fn.expand('~/Repository/nvim-mado-scratch'),
  -- dir = vim.fn.expand('~/Repository/nvim-mado-scratch/main'),
  dependencies = { 'nvim-lua/plenary.nvim' },
  opts = {
    default_file_ext = 'md',
    default_open_method = { method = 'sp', height = 15 },
    auto_hide_buffer = {
      when_tmp_buffer = true,  -- Auto-hide temporary buffers
      when_file_buffer = true, -- Auto-hide persistent buffers
    },
  },
}
