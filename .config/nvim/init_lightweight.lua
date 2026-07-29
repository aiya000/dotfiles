---Lightweight版 init.lua。editpromptなど起動速度重視の用途向け。
---tmux経由: ~/.dotfiles/.tmux.conf L99

vim.g.nvim_lightweight_mode = true

local nvim = require('nvim-lightweight')
local fn = require('utils.functions')
local git = require('git')

local s = fn.s

Require = nvim.reload_modules

-------------------
-- Global values --
-------------------

InitLua = InitLua
  or {
    loaded = false,
    neovim_home = vim.fn.expand('~/.config/nvim'),
    path_at_started = vim.fn.getcwd(),
    is_wsl = vim.fn.executable('uname') == 1 and vim.fn.system('uname -a'):match('microsoft%-standard'),
    is_unix = vim.fn.has('unix') == 1,
    is_macos = vim.fn.has('macunix') == 1,
    git_root = nil,
    memo_path = '~/memo.md',
    hydra = {},
    allowed_modeline_options = { 'filetype', 'foldmethod' },
  }

vim.schedule(function()
  git.read_git_root(function(git_root)
    InitLua.git_root = git_root
  end)
end)

InitLua.open_on_gui = InitLua.is_macos and 'open'
  or InitLua.is_wsl and 'wslview'
  or InitLua.is_unix and 'xdg-open'
  or error('init_lightweight.lua: No method found for opening GUI')

local backupdir = vim.fn.expand('~/.backup/neovim-backup')
InitLua.backupdir = backupdir
InitLua.directory = s('{backupdir}/swp', { backupdir = backupdir })
InitLua.undodir = s('{backupdir}/undo', { backupdir = backupdir })
InitLua.viewdir = s('{backupdir}/view', { backupdir = backupdir })
InitLua.sessiondir = s('{backupdir}/session', { backupdir = backupdir })

InitLua.canceler_keys_for_my_operator_surround = {
  '',
  '',
  '',
}

-- vim.opt
vim.opt.modeline = false
vim.opt.autoindent = true
vim.opt.backspace = { 'indent', 'eol', 'start' }
vim.opt.breakindent = true
vim.opt.cindent = true
vim.opt.cmdheight = 1
vim.opt.completeopt = { 'menu', 'menuone', 'noselect' }
vim.opt.conceallevel = 0
vim.opt.expandtab = true
vim.opt.fileencodings = {
  'ucs-bom', 'utf-8', 'sjis', 'euc-jp', 'cp932', 'iso-2022-jp-3',
  'iso-2022-jp', 'eucjp-ms', 'euc-jisx0213', 'ucs-bom', 'latin1', 'default',
}
vim.opt.hidden = true
vim.opt.history = 10000
vim.opt.hlsearch = true
vim.opt.incsearch = true
vim.opt.laststatus = 3
vim.opt.linebreak = true
vim.opt.list = false
vim.opt.listchars = { tab = '»_', trail = '_', extends = '»', precedes = '«', nbsp = '%', eol = '↲' }
vim.opt.matchpairs:append({ '<:>', '（:）', '｛:｝', '「:」', '＜:＞', '『:』', '【:】' })
vim.opt.joinspaces = false
vim.opt.timeout = false
vim.opt.wrap = false
vim.opt.wrapscan = false
vim.opt.number = true
vim.opt.omnifunc = 'v:lua.vim.lsp.omnifunc'
vim.opt.path = { '.', ',', './*' }
vim.opt.previewheight = 40
vim.opt.relativenumber = true
vim.opt.scrolloff = 16
vim.opt.sessionoptions = { 'buffers', 'tabpages', 'localoptions', 'winsize', 'winpos' }
vim.opt.shiftwidth = 2
vim.opt.suffixes = {}
vim.opt.tabstop = 2
vim.opt.shada = "'400,<50,s10,h"
vim.opt.visualbell = true
vim.opt.wildignorecase = true
vim.opt.wildmenu = true
vim.opt.ambiwidth = 'single'
vim.opt.background = 'dark'
vim.opt.showtabline = 2
vim.opt.mouse = ''
vim.opt.fillchars = { vert = '|', fold = ' ' }

vim.opt.foldenable = true
vim.opt.foldopen = { 'search', 'jump', 'mark', 'percent', 'insert', 'tag', 'undo' }
vim.opt.foldlevel = 99
vim.opt.foldmethod = 'expr'
vim.opt.foldexpr = 'v:lua.vim.treesitter.foldexpr()'
vim.opt.foldtext = ''

vim.opt.directory = InitLua.directory
vim.opt.viewdir = InitLua.viewdir
vim.opt.undofile = true
vim.opt.undodir = InitLua.undodir
vim.opt.backup = false

vim.opt.ignorecase = true
vim.opt.infercase = false
vim.opt.iminsert = 0

---@diagnostic disable-next-line: undefined-field
vim.opt.tags:append({
  'tags', '.git/tags',
  s('{path_at_started}/tags', { path_at_started = InitLua.path_at_started }),
  s('{path_at_started}/.git/tags', { path_at_started = InitLua.path_at_started }),
})

vim.g.mapleader = "'"
vim.g.maplocalleader = "'"

vim.g.tex_flavor = 'latex'

if InitLua.is_wsl then
  vim.g.clipboard = {
    name = 'WslClipboard',
    copy = { ['+'] = 'wsl-copy', ['*'] = 'wsl-copy' },
    paste = { ['+'] = 'wsl-paste', ['*'] = 'wsl-paste' },
    cache_enabled = 1,
  }
end

vim.env.BASH_TOYS_DUSTBOX_DIR = vim.fn.expand('~/.backup/dustbox')

-------------
-- Prepare --
-------------

if not InitLua.loaded then
  vim.opt.fileencoding = 'utf-8'
  vim.opt.encoding = 'utf-8'
end

vim.defer_fn(function()
  nvim.make_directory_if_missing(InitLua.backupdir)
  nvim.make_directory_if_missing(InitLua.directory)
  nvim.make_directory_if_missing(InitLua.undodir)
  nvim.make_directory_if_missing(InitLua.sessiondir)
end, 100)

local init_private_lua = vim.fn.expand('~/.dotfiles/.private/nvim_init_private.lua')
if vim.fn.filereadable(init_private_lua) == 1 then
  vim.cmd.source(init_private_lua)
end

local init_env_lua = vim.fn.expand('~/.config/nvim/init_env.lua')
if vim.fn.filereadable(init_env_lua) == 1 then
  vim.cmd.source(init_env_lua)
end

if vim.g.neovide then
  vim.opt.guifont = 'Hack Nerd Font:h11'
  vim.opt.mouse = 'nvi'
  vim.g.neovide_opacity = 0.8
  vim.g.transparency = 0.8
end

require('lazy-nvim-config')   -- vim.g.nvim_lightweight_mode=true により plugins-lightweight を読む
require('luarocks')
require('autocmds')
require('keymaps-lightweight')
require('commands-lightweight')
require('color-presets')

local init_env_post_lua = vim.fn.expand('~/.config/nvim/init_env_post.lua')
if vim.fn.filereadable(init_env_post_lua) == 1 then
  vim.cmd.source(init_env_post_lua)
end

vim.cmd('filetype plugin indent on')
vim.cmd('syntax enable')
InitLua.loaded = true

-- vim: foldmethod=marker foldlevel=0
