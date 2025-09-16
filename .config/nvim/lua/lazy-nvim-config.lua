---Original: https://lazy.folke.io/installation

local lazypath = vim.fn.stdpath('data') .. '/lazy/lazy.nvim'
if not (vim.uv or vim.loop).fs_stat(lazypath) then
  local lazyrepo = 'https://github.com/folke/lazy.nvim.git'
  local out = vim.fn.system({ 'git', 'clone', '--filter=blob:none', '--branch=stable', lazyrepo, lazypath })
  if vim.v.shell_error ~= 0 then
    vim.api.nvim_echo({
      { 'Failed to clone lazy.nvim:\n', 'ErrorMsg' },
      { out, 'WarningMsg' },
      { '\nPress any key to exit...' },
    }, true, {})
    vim.fn.getchar()
    os.exit(1)
  end
end
vim.opt.rtp:prepend(lazypath)

if vim.g.mapleader == nil or vim.g.maplocalleader == nil then
  error('non nil vim.g.mapleader and vim.g.maplocalleader required by lazy.nvim')
end

require('lazy').setup({
  spec = {
    { import = 'plugins' },
  },
  install = { colorscheme = { 'catppuccin' } },
  checker = {
    enabled = true,
    notify = false,  -- アップデート通知を無効化
  },
})
