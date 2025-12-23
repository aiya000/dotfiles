return {
  'aiya000/nvim-mado-scratch',
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

-- return {
--   -- dir = vim.env.HOME .. '/Repository/nvim-mado-scratch/copilot/fix-file-content-loss',
--   -- dir = vim.env.HOME .. '/Repository/nvim-mado-scratch/main',
--   dir = vim.env.HOME .. '/Repository/nvim-mado-scratch',
--   opts = {
--     file_pattern = {
--       when_file_buffer = vim.fn.expand('~/tmp/scratch-%d'),
--     },
--     default_file_ext = 'md',
--     default_open_method = {
--       method = 'float-aspect',
--       scale = { width = 0.8, height = 0.9 },
--     },
--   },
-- }
