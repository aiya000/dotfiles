return {
  'aiya000/nvim-mado-scratch',
  -- dir = vim.fn.expand('~/Repository/nvim-mado-scratch'),
  -- dir = vim.fn.expand('~/Repository/nvim-mado-scratch/main'),
  dependencies = { 'nvim-lua/plenary.nvim' },
  opts = {
    default_file_ext = 'md',
    default_open_method = {
      method = 'float-aspect',
      scale = {
        width = 0.8,
        height = 0.8,
      },
    },
  },
}
