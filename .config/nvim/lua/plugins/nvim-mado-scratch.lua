return {
  -- 'aiya000/nvim-mado-scratch',
  -- dir = vim.fn.expand('~/Repository/nvim-mado-scratch'),
  -- dir = vim.fn.expand('~/Repository/nvim-mado-scratch/main'),
  dir = vim.fn.expand('~/Repository/nvim-mado-scratch/copilot/show-file-name-in-float-window'),
  opts = {
    file_pattern = {
      when_file_buffer = vim.fn.expand('~/tmp/scratch-%d'),
    },
    default_file_ext = 'md',
    default_open_method = {
      method = 'float-aspect',
      scale = { width = 0.8, height = 0.9 },
    },
  },
}
