-- Minimal init file for running tests with plenary.nvim

-- Add the current config directory to package.path
vim.opt.rtp:append('.')

-- Make sure plenary is available
vim.cmd('set rtp+=~/.local/share/nvim/lazy/plenary.nvim')

-- Basic Neovim settings
vim.opt.swapfile = false
vim.opt.compatible = false
